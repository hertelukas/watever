/*
 * Copied from
 * https://github.com/WebAssembly/wabt/blob/main/include/wabt/opcode.h
 */

#pragma once

#include "watever/feature.hpp"
#include "watever/opcode-code-table.h"
#include "watever/type.hpp"
#include "watever/utils.hpp"
#include <llvm/Support/raw_ostream.h>

namespace watever {
struct Opcode {
  // Opcode enumerations.
  //
  // NOTE: this enum does not match the binary encoding.
  //
  enum Enum : uint32_t {
#define WATEVER_OPCODE(rtype, type1, type2, type3, mem_size, prefix, code,     \
                       Name, text, decomp)                                     \
  Name,
#include "watever/opcode.def"
#undef WATEVER_OPCODE
    Invalid,
  };

// Static opcode objects.
#define WATEVER_OPCODE(rtype, type1, type2, type3, mem_size, prefix, code,     \
                       Name, text, decomp)                                     \
  static Opcode Name##_Opcode;
#include "watever/opcode.def"
#undef WATEVER_OPCODE

  Opcode() = default; // Provided so Opcode can be member of a union.
  Opcode(Enum E) : E(E) {}
  operator Enum() const { return E; }

  static Opcode fromCode(uint32_t);
  static Opcode fromCode(uint8_t Prefix, uint32_t Code);
  [[nodiscard]] bool hasPrefix() const { return getInfo().Prefix != 0; }
  [[nodiscard]] uint8_t getPrefix() const { return getInfo().Prefix; }
  [[nodiscard]] uint8_t getCode() const { return getInfo().Code; }
  // size_t getLength() const { return getBytes().size(); }
  [[nodiscard]] const char *getName() const { return getInfo().Name; }
  [[nodiscard]] const char *getDecomp() const {
    return *getInfo().Decomp ? getInfo().Decomp : getInfo().Name;
  }
  [[nodiscard]] ValType getResultType() const { return getInfo().ResultType; }
  [[nodiscard]] ValType getParamType1() const {
    return getInfo().ParamTypes[0];
  }
  [[nodiscard]] ValType getParamType2() const;
  [[nodiscard]] ValType getParamType3() const {
    return getInfo().ParamTypes[2];
  }
  [[nodiscard]] ValType getParamType(int N) const {
    return getInfo().ParamTypes[N - 1];
  }

  // Write the opcode to OS
  void writeBytes(llvm::raw_ostream &OS) const;

  // Get the lane count of an extract/replace simd op.
  [[nodiscard]] uint32_t getSimdLaneCount() const;

  static bool isPrefixByte(uint8_t Byte) {
    return Byte == KMathPrefix || Byte == KThreadsPrefix || Byte == KSimdPrefix;
  }

  [[nodiscard]] bool isEnabled(const Features &Fs) const;
  [[nodiscard]] bool isInvalid() const;

private:
  static constexpr uint32_t KMathPrefix = 0xfc;
  static constexpr uint32_t KThreadsPrefix = 0xfe;
  static constexpr uint32_t KSimdPrefix = 0xfd;

  struct Info {
    const char *Name;
    const char *Decomp;
    ValType ResultType;
    ValType ParamTypes[3];
    uint64_t MemorySize;
    uint8_t Prefix;
    uint32_t Code;
    uint32_t PrefixCode; // See PrefixCode below. Used for fast lookup.
  };

  static uint32_t prefixCode(uint8_t Prefix, uint32_t Code) {
    if (Code >= (1 << MAX_OPCODE_BITS)) {
      // Clamp to (2^bits - 1), since we know that it is an invalid code.
      Code = (1 << MAX_OPCODE_BITS) - 1;
    }
    return (Prefix << MAX_OPCODE_BITS) | Code;
  }

  // The Opcode struct only stores an enumeration (Opcode::Enum) of all valid
  // opcodes, densely packed. We want to be able to store invalid opcodes as
  // well, for display to the user. To encode these, we use PrefixCode() to
  // generate a uint32_t of the prefix/code pair, then negate the value so it
  // doesn't overlap with the valid enum values. The negation is done using
  // `~code + 1` since prefix_code is unsigned, and MSVC warns if you use - on
  // an unsigned value.
  //
  // | 0             | Opcode::Invalid         | INT32_MAX+1    UINT32_MAX |
  // |---------------|-------------------------|---------------------------|
  // | valid opcodes |      unused space       |      invalid opcodes      |
  //
  static Enum encodeInvalidOpcode(uint32_t PrefixCode) {
    Enum Result = static_cast<Enum>(~PrefixCode + 1);
    assert(Result >= Invalid);
    return Result;
  }

  static void decodeInvalidOpcode(Enum E, uint8_t *OutPrefix,
                                  uint32_t *OutCode) {
    uint32_t PrefixCode = ~static_cast<uint32_t>(E) + 1;
    *OutPrefix = PrefixCode >> MAX_OPCODE_BITS;
    *OutCode = PrefixCode & 0xff;
  }

  [[nodiscard]] Info getInfo() const;
  static Info Infos[];

  Enum E;
};

// static
inline Opcode Opcode::fromCode(uint32_t Code) { return fromCode(0, Code); }

// static
inline Opcode Opcode::fromCode(uint8_t Prefix, uint32_t Code) {
  uint32_t PrefixCode = prefixCode(Prefix, Code);

  [[likely]]
  if (PrefixCode < WATEVER_ARRAY_SIZE(WateverOpcodeCodeTable)) {
    uint32_t Value = WateverOpcodeCodeTable[PrefixCode];
    // The default value in the table is 0. That's a valid value, but only if
    // the code is 0 (for nop).
    [[likely]]
    if (Value != 0 || Code == 0) {
      return {static_cast<Enum>(Value)};
    }
  }

  return {encodeInvalidOpcode(PrefixCode)};
}

} // namespace watever
