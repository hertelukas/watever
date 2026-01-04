/*
 * Copied from
 * https://github.com/WebAssembly/wabt/blob/main/include/wabt/feature.h
 */
#pragma once

namespace watever {

class Features {
public:
  void enableAll() {
#define WATEVER_FEATURE(variable, flag, default_, help) enable_##variable();
#include "watever/feature.def"
#undef WATEVER_FEATURE
  }

#define WATEVER_FEATURE(variable, flag, default_, help)                        \
  bool variable##_enabled() const { return variable##_enabled_; }              \
  void enable_##variable() { set_##variable##_enabled(true); }                 \
  void disable_##variable() { set_##variable##_enabled(false); }               \
  void set_##variable##_enabled(bool value) { variable##_enabled_ = value; }
#include "watever/feature.def"
#undef WATEVER_FEATURE

  // Enables all features enabled in \p Other
  void merge(Features &Other) {
#define WATEVER_FEATURE(variable, flag, default_, help)                        \
  if (Other.variable##_enabled_)                                               \
    enable_##variable();
#include "watever/feature.def"
#undef WATEVER_FEATURE
  }

private:
#define WATEVER_FEATURE(variable, flag, default_, help)                        \
  bool variable##_enabled_ = default_;
#include "watever/feature.def"
#undef WATEVER_FEATURE
};

} // namespace watever
