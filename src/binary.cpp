#include "watever/binary.hpp"
#include "watever/import.hpp"
#include "watever/ir.hpp"
#include "watever/linking.hpp"
#include "watever/opcode.hpp"
#include "watever/utils.hpp"
#include <cstdint>
#include <llvm/ADT/SmallVector.h>
#include <llvm/Support/LEB128.h>
#include <llvm/Support/raw_ostream.h>
#include <memory>
#include <variant>

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
    Inst.write(OS);
  }
}
void CodeWriter::visit(WasmBr &Br) {
  Opcode(Opcode::Enum::Br).writeBytes(OS);
  llvm::encodeULEB128(Br.Nesting, OS);
}

void BinaryWriter::writeTypes() {
  // Prepare the content of the section
  llvm::SmallVector<char> Content;
  llvm::raw_svector_ostream ContentOS(Content);
  // list(type)
  llvm::encodeULEB128(Mod.Types.size(), ContentOS);

  uint32_t TypeIndex{};
  for (auto &[_, value] : Mod.Types) {
    value->Index = TypeIndex++;

    std::visit(
        Overloaded{[&](StructType &) { WATEVER_TODO("encode structype"); },
                   [&](ArrayType &) { WATEVER_TODO("encode array type"); },
                   [&](FuncType &FT) {
                     ContentOS << Type::Enum::Func;
                     // Encode args
                     llvm::encodeULEB128(FT.Args.size(), ContentOS);
                     for (const auto &Arg : FT.Args) {
                       ContentOS << Arg;
                     }
                     // Encode results
                     llvm::encodeULEB128(FT.Results.size(), ContentOS);
                     for (const auto &Res : FT.Results) {
                       ContentOS << Res;
                     }
                   }

        },
        value->Composite);
    // TODO support rec types
  }

  OS << static_cast<uint8_t>(Section::Type);
  llvm::encodeULEB128(Content.size(), OS);
  OS << ContentOS.str();
}

void BinaryWriter::writeFunctions() {
  llvm::SmallVector<char> Content;
  llvm::raw_svector_ostream ContentOS(Content);

  uint32_t CurrentIdx{};
  // list(typeidx)
  // TODO handle imports
  llvm::encodeULEB128(Mod.Functions.size(), ContentOS);
  for (auto &Func : Mod.Functions) {
    llvm::encodeULEB128(Func->TypePtr->Index, ContentOS);
    Func->Index = CurrentIdx++;
  }

  OS << static_cast<uint8_t>(Section::Function);
  llvm::encodeULEB128(Content.size(), OS);
  OS << ContentOS.str();
}

// TODO maybe it makes sense here to predict the size of the LEB128 encoding,
// and then patch it
void BinaryWriter::writeCode() {
  llvm::SmallVector<char> Content;
  llvm::raw_svector_ostream ContentOS(Content);

  llvm::encodeULEB128(Mod.Functions.size(), ContentOS);
  llvm::SmallVector<char> Code;
  llvm::raw_svector_ostream CodeOS(Code);
  for (const auto &F : Mod.Functions) {
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
      CodeOS << Ty;
    }
    CodeWriter CW{CodeOS};
    F->visit(CW);
    Opcode(Opcode::Enum::End).writeBytes(CodeOS);

    // Write this functions code to content stream
    llvm::encodeULEB128(Code.size(), ContentOS);
    ContentOS << CodeOS.str();
    Code.clear();
  }

  OS << static_cast<uint8_t>(Section::Code);
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
  Imports.emplace_back(
      "env", "__linear_memory",
      std::make_unique<MemType>(Limit{static_cast<uint32_t>(0)}));

  writeImports(Imports);
  writeFunctions();
  writeCode();

  Linking Link{};

  SymbolTable SymTab{};
  // TODO this is very inefficient, use constructors, don't copy everything
  // everywhere
  for (const auto &F : Mod.Functions) {
    SymInfo SymInfo{};
    SymbolName SymName{};
    SymName.Index = F->Index;
    SymName.Name = F->Name.str();
    SymInfo.Kind = SymbolKind::SYMTAB_FUNCTION;
    SymInfo.Content = SymName;
    SymTab.Infos.push_back(SymInfo);
  }

  Link.Subsections.push_back(std::make_unique<SymbolTable>(SymTab));

  writeLinking(Link);
}

} /* namespace watever */
