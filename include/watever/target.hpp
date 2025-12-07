#pragma once

#include "watever/feature.hpp"
namespace watever {
struct TargetConfig {
  bool Is64Bit;
  Features EnabledFeatures;
};
} // namespace watever
