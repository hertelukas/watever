#include "watever/binary.hpp"
#include "watever/import.hpp"
#include "watever/ir.hpp"
#include "watever/linking.hpp"
#include "watever/opcode.hpp"
#include "watever/symbol.hpp"
#include <cstdint>
#include <llvm/ADT/SmallVector.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/LEB128.h>
#include <llvm/Support/raw_ostream.h>
#include <memory>

namespace watever {

void CodeWriter::visit(WasmBlock &Block) {
  Opcode(Opcode::Enum::Block).writeBytes(OS);
  // TODO blocktype
  OS << static_cast<char>(0x40);
  Block.InnerWasm->accept(*this);
  Opcode(Opcode::Enum::End).writeBytes(OS);
};

void CodeWriter::visit(WasmLoop &Loop) {
  Opcode(Opcode::Enum::Loop).writeBytes(OS);
  // TODO blocktype
  OS << static_cast<char>(0x40);
  Loop.InnerWasm->accept(*this);
  Opcode(Opcode::Enum::End).writeBytes(OS);
}
void CodeWriter::visit(WasmIf &IfElse) {
  Opcode(Opcode::Enum::If).writeBytes(OS);
  // TODO blocktype
  OS << static_cast<char>(0x40);
  IfElse.True->accept(*this);
  Opcode(Opcode::Enum::Else).writeBytes(OS);
  IfElse.False->accept(*this);
  Opcode(Opcode::Enum::End).writeBytes(OS);
}

void CodeWriter::visit(WasmReturn &) {
  Opcode(Opcode::Enum::Return).writeBytes(OS);
}
void CodeWriter::visit(WasmSeq &Seq) {
  Seq.Flow.first->accept(*this);
  Seq.Flow.second->accept(*this);
}
void CodeWriter::visit(WasmActions &Actions) {
  for (auto &Inst : Actions.Insts) {
    Inst.write(OS, Reloc);
  }
}
void CodeWriter::visit(WasmBr &Br) {
  Opcode(Opcode::Enum::Br).writeBytes(OS);
  llvm::encodeULEB128(Br.Nesting, OS);
}

void BinaryWriter::writeTypes() {
  CurrentSection++;
  // Prepare the content of the section
  llvm::SmallVector<char> Content;
  llvm::raw_svector_ostream ContentOS(Content);
  // list(type)
  llvm::encodeULEB128(Mod.Types.size(), ContentOS);

  for (auto &FT : Mod.Types) {
    FT.encode(ContentOS);
  }

  OS << static_cast<uint8_t>(Section::Type);
  llvm::encodeULEB128(Content.size(), OS);
  OS << ContentOS.str();
}

void BinaryWriter::writeFunctions() {
  CurrentSection++;
  llvm::SmallVector<char> Content;
  llvm::raw_svector_ostream ContentOS(Content);

  // list(typeidx)
  // TODO handle imports
  llvm::encodeULEB128(Mod.Functions.size(), ContentOS);
  for (auto &Func : Mod.Functions) {
    llvm::encodeULEB128(Func->TypeIndex, ContentOS);
  }

  OS << static_cast<uint8_t>(Section::Function);
  llvm::encodeULEB128(Content.size(), OS);
  OS << ContentOS.str();
}

void BinaryWriter::writeElements() {
  if (Mod.IndirectFunctionElements.size() <= 1) {
    return;
  }
  CurrentSection++;
  llvm::SmallVector<char> Content;
  llvm::raw_svector_ostream ContentOS(Content);

  llvm::encodeULEB128(Mod.IndirectFunctionElements.size() - 1, ContentOS);

  // TODO I'm confused about this; why can a single element hold multiple
  // functions?
  // We skip the first element, as this is reserved as nullptr
  llvm::encodeULEB128(uint32_t{0}, ContentOS);
  Opcode(Opcode::I32Const).writeBytes(ContentOS);
  llvm::encodeULEB128(1, ContentOS);
  Opcode(Opcode::End).writeBytes(ContentOS);
  llvm::encodeULEB128(Mod.IndirectFunctionElements.size() - 1, ContentOS);
  for (uint32_t I = 1; I < Mod.IndirectFunctionElements.size(); ++I) {
    llvm::encodeULEB128(Mod.IndirectFunctionElements[I]->FunctionIndex,
                        ContentOS);
  }

  OS << static_cast<uint8_t>(Section::Element);
  llvm::encodeULEB128(Content.size(), OS);
  OS << ContentOS.str();
}

// TODO maybe it makes sense here to predict the size of the LEB128 encoding,
// and then patch it
void BinaryWriter::writeCode() {
  llvm::SmallVector<char> Content;
  llvm::raw_svector_ostream ContentOS(Content);

  Relocation CodeRelocation(CurrentSection++, "CODE");

  llvm::encodeULEB128(Mod.Functions.size(), ContentOS);
  llvm::SmallVector<char> Code;
  llvm::raw_svector_ostream CodeOS(Code);
  for (const auto &F : Mod.Functions) {
    size_t RelocationStart = CodeRelocation.Entries.size();
    // list(locals)
    llvm::encodeULEB128(F->Locals.size(), CodeOS);
    uint32_t CurrentLocal = F->Args;
    // Locals
    for (const auto &[Ty, LocalList] : F->Locals) {
      // Assign new locals, based on type
      for (auto &Local : LocalList) {
        Local->Index = CurrentLocal++;
      }
      llvm::encodeULEB128(LocalList.size(), CodeOS);
      CodeOS << static_cast<uint8_t>(Ty);
    }
    CodeWriter CW{CodeOS, CodeRelocation};
    F->visit(CW);
    Opcode(Opcode::Enum::End).writeBytes(CodeOS);

    // Write this functions code size to content stream
    llvm::encodeULEB128(Code.size(), ContentOS);

    // All relocations have been written from the start of this function, needs
    // correction.
    for (size_t I = RelocationStart; I < CodeRelocation.Entries.size(); ++I) {
      CodeRelocation.Entries[I].Offset += ContentOS.tell();
    }

    // Now we can write the actual code
    ContentOS << CodeOS.str();
    Code.clear();
  }

  if (!CodeRelocation.Entries.empty()) {
    Relocations.push_back(std::move(CodeRelocation));
  }

  OS << static_cast<uint8_t>(Section::Code);
  llvm::encodeULEB128(Content.size(), OS);
  OS << ContentOS.str();
}

void BinaryWriter::writeData() {
  if (Mod.Datas.empty()) {
    return;
  }
  CurrentSection++;
  llvm::SmallVector<char> Content;
  llvm::raw_svector_ostream ContentOS(Content);

  llvm::encodeULEB128(Mod.Datas.size(), ContentOS);
  uint32_t Offset{};
  for (auto *Data : Mod.Datas) {
    // TODO currently every segment is active, and results in i32.const <offset>
    // end
    ContentOS << static_cast<uint8_t>(0);
    // TODO support 64-bit
    Opcode(Opcode::I32Const).writeBytes(ContentOS);
    llvm::encodeSLEB128(Offset, ContentOS);
    Opcode(Opcode::End).writeBytes(ContentOS);

    llvm::encodeULEB128(Data->Content.size(), ContentOS);
    ContentOS.write(reinterpret_cast<const char *>(Data->Content.data()),
                    Data->Content.size());
    Offset += Data->Content.size();
  }

  OS << static_cast<uint8_t>(Section::Data);
  llvm::encodeULEB128(Content.size(), OS);
  OS << ContentOS.str();
}

void BinaryWriter::writeDataCount() {
  if (Mod.Datas.empty()) {
    return;
  }
  CurrentSection++;
  OS << static_cast<uint8_t>(Section::DataCount);
  llvm::encodeULEB128(llvm::getULEB128Size(Mod.Datas.size()), OS);
  llvm::encodeULEB128(Mod.Datas.size(), OS);
}

void BinaryWriter::write() {
  writeMagic();
  writeVersion();

  writeTypes();

  llvm::SmallVector<Import, 2> Imports;

  // Add default memory
  // TODO respect bit width
  Imports.emplace_back("env", "__linear_memory",
                       std::make_unique<MemType>(Limit{uint32_t{0}}));

  for (const auto &F : Mod.Imports) {
    Imports.emplace_back(
        "env", F->Name,
        std::make_unique<FuncExternType>(FuncExternType{F->TypeIndex}));
  }

  for (const auto &G : Mod.ImportedGlobals) {
    Imports.emplace_back(G->ModuleName, G->ItemName,
                         std::make_unique<GlobalType>(G->Type, G->Mutable));
  }

  if (Mod.IndirectFunctionTable) {
    Imports.emplace_back(
        "env", Mod.IndirectFunctionTable->ItemName,
        std::make_unique<TableType>(Mod.IndirectFunctionTable->Type,
                                    Limit{uint32_t{1}}));
  }

  writeImports(Imports);
  writeFunctions();
  writeElements();
  writeDataCount();
  writeCode();
  writeData();

  Linking Link{};

  SymbolTable SymTab{};
  for (const auto &Symbol : Mod.Symbols) {

    if (llvm::isa<Function>(Symbol) || llvm::isa<Global>(Symbol) ||
        llvm::isa<Table>(Symbol)) {
      std::string Name{};
      uint32_t Index{};
      bool Import{};
      if (const auto *DF = llvm::dyn_cast<DefinedFunc>(Symbol.get())) {
        Name = DF->Name;
        Index = DF->FunctionIndex;
      } else if (const auto *IF = llvm::dyn_cast<ImportedFunc>(Symbol.get())) {
        Index = IF->FunctionIndex;
        Import = true;
      }
      // TODO defined globals might not need an entry in the symbol table.
      // However, I currently give every symbol, including defined globals, a
      // fixed index, so I need to list them in the SymTab
      else if (const auto *DG = llvm::dyn_cast<DefinedGlobal>(Symbol.get())) {
        Index = DG->GlobalIdx;
      } else if (const auto *IG =
                     llvm::dyn_cast<ImportedGlobal>(Symbol.get())) {
        Index = IG->GlobalIdx;
        Import = true;
      } else if (const auto *UT =
                     llvm::dyn_cast<UndefinedTable>(Symbol.get())) {
        Index = UT->TableIndex;
        Import = true;
      }

      SymTab.Infos.emplace_back(SymbolName(Index, std::move(Name), Import),
                                Symbol->getKind(), Symbol->LinkerFlags);
    } else if (const auto *DD = llvm::dyn_cast<DefinedData>(Symbol.get())) {
      WATEVER_TODO("use correct offset and size for defined data symbol");
      SymTab.Infos.emplace_back(SymbolData(DD->Name, DD->DataIndex, 0, 0),
                                Symbol->getKind(), Symbol->LinkerFlags);
    } else if (const auto *UD = llvm::dyn_cast<UndefinedData>(Symbol.get())) {
      SymTab.Infos.emplace_back(SymbolData(UD->Name), Symbol->getKind(),
                                Symbol->LinkerFlags);
    }
  }

  Link.Subsections.push_back(std::make_unique<SymbolTable>(SymTab));

  writeLinking(Link);

  for (const auto &Reloc : Relocations) {
    writeRelocation(Reloc);
  }
}

} /* namespace watever */
