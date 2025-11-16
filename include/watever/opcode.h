/*
 * Copied from
 * https://github.com/WebAssembly/wabt/blob/main/include/wabt/opcode.h
 */

#ifndef WATEVER_OPCODE_H
#define WATEVER_OPCODE_H

#include "watever/feature.h"
#include "watever/opcode-code-table.h"
#include "watever/type.h"
#include "watever/utils.h"

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
  bool hasPrefix() const { return getInfo().Prefix != 0; }
  uint8_t getPrefix() const { return getInfo().Prefix; }
  uint32_t getCode() const { return getInfo().Code; }
  size_t getLength() const { return getBytes().size(); }
  const char *getName() const { return getInfo().Name; }
  const char *getDecomp() const {
    return *getInfo().Decomp ? getInfo().Decomp : getInfo().Name;
  }
  Type getResultType() const { return getInfo().ResultType; }
  Type getParamType1() const { return getInfo().ParamTypes[0]; }
  Type getParamType2() const { return getInfo().ParamTypes[1]; }
  Type getParamType3() const { return getInfo().ParamTypes[2]; }
  Type getParamType(int N) const { return getInfo().ParamTypes[N - 1]; }
  Address getMemorySize() const { return getInfo().MemorySize; }

  // Get the byte sequence for this opcode, including prefix.
  std::vector<uint8_t> getBytes() const;

  // Get the lane count of an extract/replace simd op.
  uint32_t getSimdLaneCount() const;

  // Return 1 if |alignment| matches the alignment of |opcode|, or if
  // |alignment| is WABT_USE_NATURAL_ALIGNMENT.
  bool isNaturallyAligned(Address Alignment) const;

  // If |alignment| is WABT_USE_NATURAL_ALIGNMENT, return the alignment of
  // |opcode|, else return |alignment|.
  Address getAlignment(Address Alignment) const;

  static bool isPrefixByte(uint8_t Byte) {
    return Byte == KMathPrefix || Byte == KThreadsPrefix || Byte == KSimdPrefix;
  }

  bool isEnabled(const Features &Fs) const;
  bool isInvalid() const { return E >= Invalid; }

private:
  static constexpr uint32_t KMathPrefix = 0xfc;
  static constexpr uint32_t KThreadsPrefix = 0xfe;
  static constexpr uint32_t KSimdPrefix = 0xfd;

  struct Info {
    const char *Name;
    const char *Decomp;
    Type ResultType;
    Type ParamTypes[3];
    Address MemorySize;
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

  Info getInfo() const;
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
      return Opcode(static_cast<Enum>(Value));
    }
  }

  return Opcode(encodeInvalidOpcode(PrefixCode));
}

} // namespace watever

#endif // WATEVER_OPCODE_H
