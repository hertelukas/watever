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

void CodeWriter::visit(WasmActions &Actions) {
  for (auto &Inst : Actions.Insts) {
    Inst.write(OS, Reloc, LocalMap);
  }
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

  // Only one element; the entries of a function table
  llvm::encodeULEB128(1, ContentOS);

  // Use active element segment for implicit table 0
  llvm::encodeULEB128(uint32_t{0}, ContentOS);
  // We skip the first element, as this is reserved as nullptr:
  // Start writing table entries of function table at index 1
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
    uint32_t CurrentLocal = F->TotalArgs;
    // Locals
    llvm::SmallVector<uint32_t> LocalMapping(F->TotalLocals + F->TotalArgs);
    // Arguments are mapped onto themselves
    for (uint32_t I = 0; I < F->TotalArgs; ++I) {
      LocalMapping[I] = I;
    }
    for (const auto &[Ty, LocalList] : F->Locals) {
      // Assign local indices, based on type
      for (auto &Local : LocalList) {
        LocalMapping[Local] = CurrentLocal++;
      }
      llvm::encodeULEB128(LocalList.size(), CodeOS);
      CodeOS << static_cast<uint8_t>(Ty);
    }
    CodeWriter CW{CodeOS, CodeRelocation, LocalMapping};
    CW.visit(F->Body);
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
  Relocation DataRelocation(CurrentSection++, "DATA");
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
    for (auto &Relocation : Data->Relocations) {
      Relocation->Offset += ContentOS.tell();
      DataRelocation.Entries.push_back(*Relocation);
    }

    ContentOS.write(reinterpret_cast<const char *>(Data->Content.data()),
                    Data->Content.size());

    Offset += Data->Content.size();
  }

  if (!DataRelocation.Entries.empty()) {
    Relocations.push_back(std::move(DataRelocation));
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

void BinaryWriter::writeFeatures() {
  llvm::SmallVector<char> Content;
  llvm::raw_svector_ostream ContentOS(Content);

  llvm::encodeULEB128(15, ContentOS);
  ContentOS << "target_features";

  uint32_t FeatureCount = 0;
#define WATEVER_FEATURE(VAR, NAME, DEFAULT, HELP)                              \
  if (Mod.Config.EnabledFeatures.VAR##_enabled())                              \
    FeatureCount++;
#include "watever/feature.def"
#undef WATEVER_FEATURE

  llvm::encodeULEB128(FeatureCount, ContentOS);

#define WATEVER_FEATURE(VAR, NAME, DEFAULT, HELP)                              \
  {                                                                            \
    if (Mod.Config.EnabledFeatures.VAR##_enabled()) {                          \
      ContentOS << "+";                                                        \
      llvm::encodeULEB128(llvm::StringRef(NAME).size(), ContentOS);            \
      ContentOS << NAME;                                                       \
    }                                                                          \
  }
#include "watever/feature.def"
#undef WATEVER_FEATURE

  OS << static_cast<uint8_t>(Section::Custom);
  llvm::encodeULEB128(Content.size(), OS);
  OS << ContentOS.str();
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
      } else if (const auto *AF = llvm::dyn_cast<AliasedFunc>(Symbol.get())) {
        Name = AF->Name;
        Index = AF->FunctionIndex;
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
      SymTab.Infos.emplace_back(
          SymbolData(DD->Name, DD->DataIndex, 0, DD->Content.size()),
          Symbol->getKind(), Symbol->LinkerFlags);
    } else if (const auto *UD = llvm::dyn_cast<UndefinedData>(Symbol.get())) {
      SymTab.Infos.emplace_back(SymbolData(UD->Name), Symbol->getKind(),
                                Symbol->LinkerFlags);
    }
  }

  Link.Subsections.push_back(std::make_unique<SymbolTable>(SymTab));

  if (!Mod.Datas.empty()) {
    SegmentInfo SI;
    for (auto *Data : Mod.Datas) {
      SI.Segments.emplace_back(dataSectionToString(Data->Sec) + Data->Name,
                               Data->Alignment, Data->Flags);
    }
    Link.Subsections.push_back(std::make_unique<SegmentInfo>(SI));
  }

  if (!Mod.InitFunctions.empty()) {
    InitFunctions IFs;
    for (auto &[F, Priority] : Mod.InitFunctions) {
      auto *WasmFn = Mod.FunctionMap.lookup(F);
      assert(WasmFn && "could not find lowered init function");
      InitFunc I(Priority, WasmFn->SymbolIndex);
      IFs.Functions.emplace_back(Priority, WasmFn->SymbolIndex);
    }
    Link.Subsections.push_back(std::make_unique<InitFunctions>(IFs));
  }

  writeLinking(Link);

  for (const auto &Reloc : Relocations) {
    writeRelocation(Reloc);
  }

  writeFeatures();
}

} /* namespace watever */
