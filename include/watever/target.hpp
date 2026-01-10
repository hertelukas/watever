#pragma once

#include "watever/feature.hpp"
namespace watever {
struct TargetConfig {
  Features EnabledFeatures;
  bool DoColoring = true;
};
} // namespace watever
