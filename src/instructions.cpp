#include "watever/instructions.hpp"
#include "watever/symbol.hpp"
#include "watever/utils.hpp"
#include <llvm/Support/LEB128.h>

using namespace watever;
#ifdef WATEVER_LOGGING

void WasmInst::dump(llvm::raw_ostream &OS,
                    const llvm::SmallVector<llvm::SmallVector<uint32_t>, 0>
                        &BranchTables) const {
  OS << Opcode(Op).getName();

  std::visit(
      Overloaded{
          [&](std::monostate) {},
          [&](int64_t Imm) { OS << " " << Imm; },
          [&](uint64_t Imm) { OS << " " << Imm; },
          [&](float Imm) { OS << " " << Imm; },
          [&](double Imm) { OS << " " << Imm; },
          [&](const MemArg &A) {
            OS << llvm::formatv(" align: {}, idx: {}, offset: {}", A.Alignment,
                                A.MemIdx, A.Offset);
          },
          [&](const LocalArg &A) { OS << " " << A.Index; },
          [&](const BranchTableArg &A) {
            auto &BranchTable = BranchTables[A.TableIdx];
            OS << llvm::formatv(
                " [{0:$[, ]}] : {1}",
                llvm::make_range(BranchTable.begin(), BranchTable.end()),
                A.DefaultTarget);
          },
          [&](const RelocatableFuncArg &A) {
            OS << " " << A.Func->FunctionIndex;
          },
          [&](const RelocatableGlobalArg &A) { OS << " " << A.Gl->GlobalIdx; },
          [&](const RelocatablePointerArg &A) { OS << " " << A.DT->Name; },
          [&](const RelocatableIndirectCallArg &A) {
            OS << " " << A.TypeIndex << " " << A.Tab->TableIndex;
          },
          [&](const RelocatableTableIndexArg &A) {
            OS << " table index for " << A.F->Name;
          },
          [&](const MemCpyArg &A) {
            OS << " " << A.FromMemory << " " << A.ToMemory;
          },
          [&](const BlockTypeArg &A) { OS << " " << toString(A.Type); },
      },
      Arg);
}
#endif

void WasmInst::write(llvm::raw_ostream &OS, Relocation &Reloc,
                     llvm::ArrayRef<uint32_t> LocalMap,
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
            if (A.Index >= LocalMap.size() || LocalMap[A.Index] == ~0U) {
              WATEVER_UNREACHABLE("Could not find mapping for local {}",
                                  A.Index);
            }
            llvm::encodeULEB128(LocalMap[A.Index], OS);
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
