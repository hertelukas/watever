#pragma once

#include "watever/import.hpp"
#include "watever/ir.hpp"
#include "watever/linking.hpp"
#include <cstdint>
#include <llvm/ADT/SmallVector.h>
#include <llvm/Support/Endian.h>
#include <llvm/Support/LEB128.h>
#include <llvm/Support/raw_ostream.h>

namespace watever {
static constexpr uint32_t WateverBinaryMagic = 0x6d736100;
static constexpr uint32_t Version = 0x1;

class CodeWriter final : public WasmVisitor {
  Relocation &Reloc;
  llvm::raw_ostream &OS;

public:
  CodeWriter(llvm::raw_ostream &OS, Relocation &Reloc) : Reloc(Reloc), OS(OS) {}
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

  // This is needed for relocations, so they can index into the correct section
  // by index, not by section ID. (For example, if we only have a type and a
  // code section, the code section will have index  1)
  uint32_t CurrentSection;

  llvm::SmallVector<Relocation> Relocations;

  void writeMagic() { writeIntegral(WateverBinaryMagic, OS); }
  void writeVersion() { writeIntegral(Version, OS); }

  void writeTypes();
  void writeFunctions();
  void writeCode();

  // Custom section with name "reloc.<SECTION>"
  // TODO figure out if this is a custom section or part of linking custom
  // section
  void writeRelocation(const Relocation &Rel) {
    llvm::SmallVector<char> Content;
    llvm::raw_svector_ostream ContentOS(Content);

    llvm::encodeULEB128(Rel.SectionName.length() + 6, ContentOS);
    ContentOS << "reloc.";
    ContentOS << Rel.SectionName;
    llvm::encodeULEB128(Rel.Section, ContentOS);
    llvm::encodeULEB128(Rel.Entries.size(), ContentOS);

    for (auto &Entry : Rel.Entries) {
      ContentOS << static_cast<uint8_t>(Entry.Type);
      llvm::encodeULEB128(Entry.Offset, ContentOS);
      llvm::encodeULEB128(Entry.Index, ContentOS);
    }

    OS << static_cast<uint8_t>(Section::Custom);
    llvm::encodeULEB128(Content.size(), OS);
    OS << ContentOS.str();
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

  void writeImports(llvm::ArrayRef<Import> Imports) {
    if (Imports.empty()) {
      return;
    }
    CurrentSection++;
    llvm::SmallVector<char> Content;
    llvm::raw_svector_ostream ContentOS(Content);

    llvm::encodeULEB128(Imports.size(), ContentOS);
    for (const auto &Import : Imports) {
      llvm::encodeULEB128(Import.ModuleName.size(), ContentOS);
      ContentOS << Import.ModuleName;
      llvm::encodeULEB128(Import.ItemName.size(), ContentOS);
      ContentOS << Import.ItemName;
      ContentOS << static_cast<uint8_t>(Import.Extern->getExternalType());
      Import.Extern->writePayload(ContentOS);
    }

    OS << static_cast<uint8_t>(Section::Import);
    llvm::encodeULEB128(Content.size(), OS);
    OS << ContentOS.str();
  }

public:
  // TODO make Module const? Currently, I'm writing indices into functions...
  BinaryWriter(llvm::raw_ostream &Stream, Module &Mod) : OS(Stream), Mod(Mod) {}
  void write();
};

} /* namespace watever */
