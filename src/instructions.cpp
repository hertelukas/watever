#include "watever/instructions.hpp"
#include "watever/symbol.hpp"
#include <llvm/Support/LEB128.h>

using namespace watever;
#ifdef WATEVER_LOGGING

[[nodiscard]] std::string WasmInst::getString(
    const llvm::SmallVector<llvm::SmallVector<uint32_t>, 0> &BranchTables)
    const {
  const char *Name = Opcode(Op).getName();

  return std::visit(
      Overloaded{
          [&](std::monostate) { return std::string(Name); },
          [&](int64_t Imm) {
            return llvm::formatv("{0} {1}", Name, Imm).str();
          },
          [&](uint64_t Imm) {
            return llvm::formatv("{0} {1}", Name, Imm).str();
          },
          [&](float Imm) { return llvm::formatv("{0} {1}", Name, Imm).str(); },
          [&](double Imm) { return llvm::formatv("{0} {1}", Name, Imm).str(); },
          [&](const MemArg &A) {
            return llvm::formatv("align: {}, idx: {}, offset: {}", A.Alignment,
                                 A.MemIdx, A.Offset)
                .str();
          },
          [&](const LocalArg &A) { return llvm::formatv("{}", A.Index).str(); },
          [&](const BranchTableArg &A) {
            auto &BranchTable = BranchTables[A.TableIdx];
            return llvm::formatv(
                       "[{0:$[, ]}] : {1}",
                       llvm::make_range(BranchTable.begin(), BranchTable.end()),
                       A.DefaultTarget)
                .str();
          },
          [&](const RelocatableFuncArg &A) {
            return llvm::formatv("{}", A.Func->FunctionIndex).str();
          },
          [&](const RelocatableGlobalArg &A) {
            return llvm::formatv("{}", A.Gl->GlobalIdx).str();
          },
          [&](const RelocatablePointerArg &A) {
            return llvm::formatv("{}", A.DT->Name).str();
          },
          [&](const RelocatableIndirectCallArg &A) {
            return llvm::formatv("{} {}", A.TypeIndex, A.Tab->TableIndex).str();
          },
          [&](const RelocatableTableIndexArg &A) {
            return llvm::formatv("table index for {}", A.F->Name).str();
          },
          [&](const MemCpyArg &A) {
            return llvm::formatv("{} {}", A.FromMemory, A.ToMemory).str();
          },
          [&](const BlockTypeArg &A) {
            return llvm::formatv("{}", toString(A.Type)).str();
          },
      },
      Arg);
}
#endif

void WasmInst::write(llvm::raw_ostream &OS, Relocation &Reloc,
                     llvm::DenseMap<uint32_t, uint32_t> &LocalMap,
                     const llvm::SmallVector<llvm::SmallVector<uint32_t>, 0>
                         &BranchTables) const {
  Opcode(Op).writeBytes(OS);

  std::visit(
      Overloaded{
          [&](std::monostate) {},
          [&](int64_t Imm) { llvm::encodeSLEB128(Imm, OS); },
          [&](uint64_t Imm) { llvm::encodeULEB128(Imm, OS); },
          [&](float Imm) {
            OS.write(reinterpret_cast<const char *>(&Imm), sizeof(Imm));
          },
          [&](double Imm) {
            OS.write(reinterpret_cast<const char *>(&Imm), sizeof(Imm));
          },
          [&](const MemArg &A) {
            if (A.Alignment >= 64) {
              WATEVER_LOG_ERR("Alignment is too large: {}", A.Alignment);
            }
            uint32_t Flag = A.Alignment;
            if (A.MemIdx != 0) {
              Flag |= 64; // Signal that we have a MemIdx
            }
            llvm::encodeULEB128(Flag, OS);
            if (A.MemIdx != 0) {
              llvm::encodeULEB128(A.MemIdx, OS);
            }
            llvm::encodeULEB128(A.Offset, OS);
          },
          [&](const LocalArg &A) {
            if (auto It = LocalMap.find(A.Index); It != LocalMap.end()) {
              llvm::encodeULEB128(It->second, OS);
            } else {
              WATEVER_LOG_WARN("Could not find mapping for local {}", A.Index);
            }
          },
          [&](const BranchTableArg &A) {
            auto &BranchTable = BranchTables[A.TableIdx];
            llvm::encodeULEB128(BranchTable.size(), OS);
            for (auto &T : BranchTable) {
              llvm::encodeULEB128(T, OS);
            }
            llvm::encodeULEB128(A.DefaultTarget, OS);
          },
          [&](const RelocatableFuncArg &A) {
            Reloc.Entries.emplace_back(
                RelocationType::R_WASM_FUNCTION_INDEX_LEB, OS.tell(),
                A.Func->SymbolIndex);
            llvm::encodeULEB128(A.Func->FunctionIndex, OS, 5);
          },
          [&](const RelocatableGlobalArg &A) {
            Reloc.Entries.emplace_back(RelocationType::R_WASM_GLOBAL_INDEX_LEB,
                                       OS.tell(), A.Gl->SymbolIndex);
            llvm::encodeULEB128(A.Gl->GlobalIdx, OS, 5);
          },
          [&](const RelocatablePointerArg &A) {
            Reloc.Entries.emplace_back(RelocationType::R_WASM_MEMORY_ADDR_SLEB,
                                       OS.tell(), A.DT->SymbolIndex);
            llvm::encodeSLEB128(0, OS, 5);
          },
          [&](const RelocatableIndirectCallArg &A) {
            Reloc.Entries.emplace_back(RelocationType::R_WASM_TYPE_INDEX_LEB,
                                       OS.tell(), A.TypeIndex);
            Reloc.Entries.emplace_back(RelocationType::R_WASM_TABLE_NUMBER_LEB,
                                       OS.tell() + 5, A.Tab->SymbolIndex);
            llvm::encodeULEB128(A.TypeIndex, OS, 5);
            llvm::encodeULEB128(A.Tab->TableIndex, OS, 5);
          },
          [&](const RelocatableTableIndexArg &A) {
            Reloc.Entries.emplace_back(RelocationType::R_WASM_TABLE_INDEX_SLEB,
                                       OS.tell(), A.F->SymbolIndex);
            llvm::encodeSLEB128(0, OS, 5);
          },
          [&](const MemCpyArg &A) {
            llvm::encodeULEB128(A.FromMemory, OS);
            llvm::encodeULEB128(A.ToMemory, OS);
          },
          [&](const BlockTypeArg &A) { OS << static_cast<uint8_t>(A.Type); },
      },
      Arg);
}
