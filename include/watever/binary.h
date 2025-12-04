#ifndef _BINARY_H
#define _BINARY_H

#include "watever/ir.h"
#include "watever/linking.h"
#include <cstdint>
#include <llvm/Support/Endian.h>
#include <llvm/Support/LEB128.h>
#include <llvm/Support/raw_ostream.h>

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
  const Module &Mod;

  void writeMagic() { writeIntegral(WateverBinaryMagic, OS); }
  void writeVersion() { writeIntegral(Version, OS); }

  void writeTypes();

  // Custom section with name "reloc.<SECTION>"
  // TODO figure out if this is a custom section or part of linking custom
  // section
  void writeRelocation(const Relocation &Rel) {
    llvm::encodeULEB128(Rel.Section, OS);
    llvm::encodeULEB128(Rel.Entries.size(), OS);

    for (auto &Entry : Rel.Entries) {
      OS << static_cast<uint8_t>(Entry.Type);
      llvm::encodeULEB128(Entry.Offset, OS);
      llvm::encodeULEB128(Entry.Index, OS);
    }
  }

  void writeLinking(const Linking &Link) {
    llvm::encodeULEB128(Link.Version, OS);
    for (const auto &SectionPtr : Link.Subsections) {
      OS << static_cast<uint8_t>(SectionPtr->getSectionType());
      llvm::encodeULEB128(SectionPtr->getPayloadLen(), OS); // payload_len
      SectionPtr->writePayload(OS);                         // payload_data
    }
  }

public:
  BinaryWriter(llvm::raw_ostream &Stream, const Module &Mod)
      : OS(Stream), Mod(Mod) {}
  void write();
};

} /* namespace watever */

#endif /* _BINARY_H */
