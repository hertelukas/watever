#include "watever/instructions.hpp"
#include "watever/symbol.hpp"

using namespace watever;

std::string RelocatableFuncArg::getString() const {
  return llvm::formatv("{}", Func->FunctionIndex);
}

void RelocatableFuncArg::encode(llvm::raw_ostream &OS) const {
  // Must be padded to 5 bytes, so it can be patched by the linker
  llvm::encodeULEB128(Func->FunctionIndex, OS, 5);
}

void RelocatableFuncArg::addRelocation(llvm::raw_ostream &OS,
                                       Relocation &Reloc) {
  Reloc.Entries.emplace_back(RelocationType::R_WASM_FUNCTION_INDEX_LEB,
                             OS.tell(), Func->SymbolIndex);
}

std::string RelocatableGlobalArg::getString() const {
  return llvm::formatv("{}", Gl->GlobalIdx);
}

void RelocatableGlobalArg::encode(llvm::raw_ostream &OS) const {
  llvm::encodeULEB128(Gl->GlobalIdx, OS, 5);
}

void RelocatableGlobalArg::addRelocation(llvm::raw_ostream &OS,
                                         Relocation &Reloc) {
  Reloc.Entries.emplace_back(RelocationType::R_WASM_GLOBAL_INDEX_LEB, OS.tell(),
                             Gl->SymbolIndex);
}

[[nodiscard]] std::string RelocatablePointerArg::getString() const {
  return llvm::formatv("{}", DT->Name);
}
void RelocatablePointerArg::encode(llvm::raw_ostream &OS) const {
  llvm::encodeSLEB128(0, OS, 5);
}

void RelocatablePointerArg::addRelocation(llvm::raw_ostream &OS,
                                          Relocation &Reloc) {
  Reloc.Entries.emplace_back(RelocationType::R_WASM_MEMORY_ADDR_SLEB, OS.tell(),
                             DT->SymbolIndex);
}

[[nodiscard]] std::string RelocatableIndirectCallArg::getString() const {
  return llvm::formatv("{} {}", TypeIndex, Tab->TableIndex).str();
}
void RelocatableIndirectCallArg::encode(llvm::raw_ostream &OS) const {
  llvm::encodeULEB128(TypeIndex, OS, 5);
  llvm::encodeULEB128(Tab->TableIndex, OS, 5);
}

void RelocatableIndirectCallArg::addRelocation(llvm::raw_ostream &OS,
                                               Relocation &Reloc) {
  Reloc.Entries.emplace_back(RelocationType::R_WASM_TYPE_INDEX_LEB, OS.tell(),
                             TypeIndex);
  Reloc.Entries.emplace_back(RelocationType::R_WASM_TABLE_NUMBER_LEB,
                             OS.tell() + 5, Tab->SymbolIndex);
}

[[nodiscard]] std::string RelocatableTableIndexArg::getString() const {
  return llvm::formatv("table index for {}", F->Name).str();
}
void RelocatableTableIndexArg::encode(llvm::raw_ostream &OS) const {
  llvm::encodeSLEB128(0, OS, 5);
}

void RelocatableTableIndexArg::addRelocation(llvm::raw_ostream &OS,
                                             Relocation &Reloc) {
  Reloc.Entries.emplace_back(RelocationType::R_WASM_TABLE_INDEX_SLEB, OS.tell(),
                             F->SymbolIndex);
}
