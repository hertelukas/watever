/*
 * Copied from
 * https://github.com/WebAssembly/wabt/blob/main/include/wabt/base-types.h
 */
#pragma once

#include <cstddef>
#include <cstdint>

namespace watever {
using Index = uint32_t;   // An index into one of the many index spaces.
using Address = uint64_t; // An address or size in linear memory.
using Offset = size_t;    // An offset into a host's file or memory buffer.

constexpr Address KInvalidAddress = ~0;
constexpr Index KInvalidIndex = ~0;
constexpr Offset KInvalidOffset = ~0;

} // namespace watever


