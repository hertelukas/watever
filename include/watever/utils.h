#ifndef WATEVER_UTILS_H
#define WATEVER_UTILS_H

#include <llvm/IR/Type.h>

/* clang-format off */
#include <llvm/Support/raw_ostream.h>
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

#define WATEVER_PANIC_IMPL(prefix, ...)                                        \
  do {                                                                         \
    WATEVER_LOG_ERR(prefix __VA_ARGS__);                                       \
    std::abort();                                                              \
  } while (0)

#define WATEVER_TODO(fmt, ...) WATEVER_LOG_WARN("[TODO] " fmt, ##__VA_ARGS__)
#define WATEVER_UNIMPLEMENTED(...)                                             \
  WATEVER_PANIC_IMPL("PANIC [UNIMPLEMENTED] ", __VA_ARGS__)

#define WATEVER_UNREACHABLE(...)                                               \
  WATEVER_PANIC_IMPL("PANIC [UNREACHABLE] ", __VA_ARGS__)

#define WATEVER_ARRAY_SIZE(a) (sizeof(a) / sizeof(a[0]))
enum { WATEVER_USE_NATURAL_ALIGNMENT = 0xFFFFFFFFFFFFFFFF };

template <typename T> std::string llvmToString(const T &V) {
  std::string S;
  llvm::raw_string_ostream OS(S);
  V.print(OS);
  return OS.str();
}
template <std::integral T> void writeIntegral(T Value, llvm::raw_ostream &OS) {
  if constexpr (std::endian::native == std::endian::little) {
    OS.write(reinterpret_cast<const char *>(&Value), sizeof(Value));
  } else {
    T LE = std::byteswap(Value);
    OS.write(reinterpret_cast<const char *>(&LE), sizeof(LE));
  }
}
template <std::integral T> static constexpr size_t getLEB128Size(T Value) {
  size_t Size = 0;
  if constexpr (std::is_unsigned_v<T>) {
    do {
      Value >>= 7;
      Size++;
    } while (Value != 0);
  } else {
    while (true) {
      std::byte Byte = static_cast<std::byte>(Value & 0x7F);
      Value >>= 7;
      Size++;

      bool SignBitClear = (Byte & std::byte{0x40}) == std::byte{0x00};
      bool SignBitSet = (Byte & std::byte{0x40}) != std::byte{0x00};

      if ((Value == 0 && SignBitClear) || (Value == -1 && SignBitSet)) {
        break;
      }
    }
  }
  return Size;
}
template <std::integral T> void writeLEB128(T Value, llvm::raw_ostream &OS) {
  if constexpr (std::is_unsigned_v<T>) {
    do {
      std::byte Byte = static_cast<std::byte>(Value & 0x7F);
      Value >>= 7;
      if (Value != 0) {
        Byte |= static_cast<std::byte>(0x80);
      }
      OS << static_cast<char>(Byte);
    } while (Value != 0);
  } else {
    while (true) {
      std::byte Byte = static_cast<std::byte>(Value & 0x7F);
      Value >>= 7;

      bool SignBitClear = (Byte & std::byte{0x40}) == std::byte{0x00};
      bool SignBitSet = (Byte & std::byte{0x40}) != std::byte{0x00};

      if ((Value == 0 && SignBitClear) || (Value == -1 && SignBitSet)) {
        OS << static_cast<char>(Byte);

        break;
      }
      Byte |= static_cast<std::byte>(0x80);
      OS << static_cast<char>(Byte);
    }
  }
}
// Writes exactly n bytes, regardless of the value
template <std::unsigned_integral T>
void writeLEB128Fixed(T Value, size_t N, llvm::raw_ostream &OS) {
  for (size_t I = 0; I < N; ++I) {
    std::byte Byte = static_cast<std::byte>(Value & 0x7F);
    Value >>= 7;

    if (I < N - 1) {
      Byte |= static_cast<std::byte>(0x80);
    }
    OS << static_cast<char>(Byte);
  }
  if (Value != 0) {
    WATEVER_LOG_ERR("value too large for fixed-width LEB128 encoding");
  }
}

#endif // WATEVER_UTILS_H
