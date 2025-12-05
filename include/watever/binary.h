#ifndef _BINARY_H
#define _BINARY_H

#include "watever/ir.h"
#include "watever/linking.h"
#include <cstdint>
#include <llvm/ADT/SmallVector.h>
#include <llvm/Support/Endian.h>
#include <llvm/Support/LEB128.h>
#include <llvm/Support/raw_ostream.h>

namespace watever {
static constexpr uint32_t WateverBinaryMagic = 0x6d736100;
static constexpr uint32_t Version = 0x1;

class CodeWriter final : public WasmVisitor {
  llvm::raw_svector_ostream &OS;

public:
  CodeWriter(llvm::raw_svector_ostream &OS) : OS(OS) {}
  void visit(WasmBlock &Block) override;
  void visit(WasmLoop &Loop) override;
  void visit(WasmIf &IfElse) override;
  void visit(WasmReturn &) override;
  void visit(WasmSeq &Seq) override;
  void visit(WasmActions &Actions) override;
  void visit(WasmBr &Br) override;
};

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
  Module &Mod;

  void writeMagic() { writeIntegral(WateverBinaryMagic, OS); }
  void writeVersion() { writeIntegral(Version, OS); }

  void writeTypes();
  void writeFunctions();
  void writeCode();

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
    llvm::SmallVector<char> Content;
    llvm::raw_svector_ostream ContentOS(Content);

    llvm::encodeULEB128(7, ContentOS);
    ContentOS << "linking";
    llvm::encodeULEB128(Link.Version, ContentOS);
    for (const auto &SectionPtr : Link.Subsections) {
      ContentOS << static_cast<uint8_t>(SectionPtr->getSectionType());
      llvm::encodeULEB128(SectionPtr->getPayloadLen(),
                          ContentOS);      // payload_len
      SectionPtr->writePayload(ContentOS); // payload_data
    }

    OS << static_cast<uint8_t>(Section::Custom);
    llvm::encodeULEB128(Content.size(), OS);
    OS << ContentOS.str();
  }

public:
  // TODO make Module const? Currently, I'm writing indices into functions...
  BinaryWriter(llvm::raw_ostream &Stream, Module &Mod) : OS(Stream), Mod(Mod) {}
  void write();
};

} /* namespace watever */

#endif /* _BINARY_H */
