#ifndef _BINARY_H
#define _BINARY_H

#include "structure.h"
#include <llvm/Support/Endian.h>
#include <llvm/Support/raw_ostream.h>
#include <bit>
#include <cstdint>

namespace watever {
static constexpr uint32_t WateverBinaryMagic = 0x6d736100;
static constexpr uint32_t Version = 0x1;

enum class Section : uint8_t {
  Custom = 0,
  Type = 1,
  Import = 2,
  Function = 3,
  Table = 4,
  Memory = 5,
  Global = 6,
  Export = 7,
  Start = 8,
  Element = 9,
  Code = 10,
  Data = 11,
  DataCount = 12,
  Tag = 13,
};

class BinaryWriter {
  llvm::raw_ostream &OS;

  template <std::integral T> void writeIntegral(T Value) {
    if constexpr (std::endian::native == std::endian::little) {
      OS.write(reinterpret_cast<const char *>(&Value), sizeof(Value));
    } else {
      T LE = std::byteswap(Value);
      OS.write(reinterpret_cast<const char *>(&LE), sizeof(LE));
    }
  }

  void writeMagic() { writeIntegral(WateverBinaryMagic); }
  void writeVersion() { writeIntegral(Version); }

public:
  BinaryWriter(llvm::raw_ostream &Stream) : OS(Stream) {}
  void write(const Module &Mod);
};

} /* namespace watever */

#endif /* _BINARY_H */
