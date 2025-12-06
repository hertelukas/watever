#pragma once

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

#define WATEVER_TODO(fmt, ...)                                                 \
  WATEVER_LOG_WARN("[TODO] " fmt __VA_OPT__(, ) __VA_ARGS__)
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

template <class... Ts> struct Overloaded : Ts... {
  using Ts::operator()...;
};
