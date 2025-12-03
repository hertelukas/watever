#ifndef _BINARY_H
#define _BINARY_H

#include "watever/ir.h"
#include "watever/linking.h"
#include <cstdint>
#include <llvm/Support/Endian.h>
#include <llvm/Support/raw_ostream.h>

namespace watever {
static constexpr uint32_t WateverBinaryMagic = 0x6d736100;
static constexpr uint32_t Version = 0x1;

class BinaryWriter {
  llvm::raw_ostream &OS;

  void writeMagic() { writeIntegral(WateverBinaryMagic, OS); }
  void writeVersion() { writeIntegral(Version, OS); }

  // Custom section with name "reloc.<SECTION>"
  // TODO figure out if this is a custom section or part of linking custom
  // section
  void writeRelocation(const Relocation &Rel) {
    writeLEB128(Rel.Section, OS);
    writeLEB128(Rel.Entries.size(), OS);

    for (auto &Entry : Rel.Entries) {
      OS << static_cast<uint8_t>(Entry.Type);
      writeLEB128(Entry.Offset, OS);
      writeLEB128(Entry.Index, OS);
    }
  }

  void writeLinking(const Linking &Link) {
    writeLEB128(Link.Version, OS);
    for (const auto &SectionPtr : Link.Subsections) {
      OS << static_cast<uint8_t>(SectionPtr->getSectionType());
      writeLEB128(SectionPtr->getPayloadLen(), OS); // payload_len
      SectionPtr->writePayload(OS);                 // payload_data
    }
  }

public:
  BinaryWriter(llvm::raw_ostream &Stream) : OS(Stream) {}
  void write(const Module &Mod);
};

} /* namespace watever */

#endif /* _BINARY_H */
