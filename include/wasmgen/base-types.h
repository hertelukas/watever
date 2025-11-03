/*
 * Copied from
 * https://github.com/WebAssembly/wabt/blob/main/include/wabt/base-types.h
 */

#ifndef WASMGEN_BASE_TYPES_H
#define WASMGEN_BASE_TYPES_H

#include <cstddef>
#include <cstdint>

namespace wasmgen {
using Index = uint32_t;   // An index into one of the many index spaces.
using Address = uint64_t; // An address or size in linear memory.
using Offset = size_t;    // An offset into a host's file or memory buffer.

constexpr Address kInvalidAddress = ~0;
constexpr Index kInvalidIndex = ~0;
constexpr Offset kInvalidOffset = ~0;

} // namespace wasmgen

#endif // WASMGEN_BASE_TYPES_H
