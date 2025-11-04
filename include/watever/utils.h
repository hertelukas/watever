#ifndef WATEVER_UTILS_H
#define WATEVER_UTILS_H

#include <concepts>
#include <cstddef>
#define WATEVER_ARRAY_SIZE(a) (sizeof(a) / sizeof(a[0]))
enum { WATEVER_USE_NATURAL_ALIGNMENT = 0xFFFFFFFFFFFFFFFF };

template <typename C>
concept ByteContainer = requires(C &Container, std::byte B) {
  { Container.push_back(B) };
};

template <std::integral T, ByteContainer Container>
void leb128(T Value, Container &OutBuffer) {
  if constexpr (std::is_unsigned_v<T>) {
    do {
      std::byte Byte = static_cast<std::byte>(Value & 0x7F);
      Value >>= 7;
      if (Value != 0) {
        Byte |= static_cast<std::byte>(0x80);
      }
      OutBuffer.push_back(Byte);
    } while (Value != 0);
  } else {
    static_assert(std::is_signed_v<T>,
                  "Integral type must be signed or unsigned");
    while (true) {
      std::byte Byte = static_cast<std::byte>(Value & 0x7F);
      Value >>= 7;

      bool SignBitClear = (Byte & std::byte{0x40}) == std::byte{0x00};
      bool SignBitSet = (Byte & std::byte{0x40}) != std::byte{0x00};

      if ((Value == 0 && SignBitClear) || (Value == -1 && SignBitSet)) {
        OutBuffer.push_back(Byte);
        break;
      }
      Byte |= static_cast<std::byte>(0x80);
      OutBuffer.push_back(Byte);
    }
  }
}

#endif // WATEVER_UTILS_H
