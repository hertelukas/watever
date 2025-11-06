#ifndef WATEVER_UTILS_H
#define WATEVER_UTILS_H

/* clang-format off */
#ifdef WATEVER_LOGGING
  #include <spdlog/spdlog.h>
  #define WATEVER_LOG(level, ...)                                                 \
    (spdlog::should_log(level) ? spdlog::log(level, __VA_ARGS__) : (void)0)
  #ifndef NDEBUG
    #define WATEVER_LOG_TRACE(...) WATEVER_LOG(spdlog::level::trace, __VA_ARGS__)
    #define WATEVER_LOG_DBG(...) WATEVER_LOG(spdlog::level::debug, __VA_ARGS__)
  #else
    #define WATEVER_LOG_TRACE(...)
    #define WATEVER_LOG_DBG(...)
  #endif
  #define WATEVER_LOG_INFO(...) WATEVER_LOG(spdlog::level::info, __VA_ARGS__)
  #define WATEVER_LOG_WARN(...) WATEVER_LOG(spdlog::level::warn, __VA_ARGS__)
  #define WATEVER_LOG_ERR(...) WATEVER_LOG(spdlog::level::err, __VA_ARGS__)
#else
  #define WATEVER_LOG_TRACE(...)
  #define WATEVER_LOG_DBG(...)
  #define WATEVER_LOG_INFO(...)
  #define WATEVER_LOG_WARN(...)
  #define WATEVER_LOG_ERR(...)
#endif
/* clang-format on */

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
