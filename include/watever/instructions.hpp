#pragma once

#include "watever/linking.hpp"
#include "watever/opcode.hpp"
#include "watever/type.hpp"
#include <cstdint>
#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/Support/FormatVariadic.h>
#include <llvm/Support/LEB128.h>
#include <llvm/Support/raw_ostream.h>

namespace watever {
struct Global;
struct ImportedGlobal;
struct Function;
struct Data;
struct Table;

struct MemArg {
  uint64_t Offset;
  uint32_t Alignment;
  uint32_t MemIdx;
}; // 16 byte

struct LocalArg {
  uint32_t Index;
}; // 4 byte

struct BranchTableArg {
  uint64_t TableIdx; // Index into central branchTableTargets
  uint32_t DefaultTarget;
}; // 12 byte

struct RelocatableFuncArg {
  Function *Func;
}; // 8 byte

struct RelocatableGlobalArg {
  Global *Gl;
}; // 8 byte

struct RelocatablePointerArg {
  Data *DT;
}; // 8 byte

struct RelocatableTableIndexArg {
  Function *F;
}; // 8 byte

struct RelocatableIndirectCallArg {
  Table *Tab;
  uint32_t TypeIndex;
}; // 12 byte

struct MemCpyArg {
  uint32_t FromMemory;
  uint32_t ToMemory;
}; // 8 byte

struct BlockTypeArg {
  ValType Type;
}; // 1 byte

class WasmInst {
  using Storage =
      std::variant<std::monostate, int64_t, uint64_t, float, double, MemArg,
                   LocalArg, BranchTableArg, RelocatableFuncArg,
                   RelocatableGlobalArg, RelocatablePointerArg,
                   RelocatableIndirectCallArg, RelocatableTableIndexArg,
                   MemCpyArg, BlockTypeArg>;

public:
  Storage Arg;
  Opcode::Enum Op;

  WasmInst(Opcode::Enum O) : Arg(std::monostate{}), Op(O) {}
  WasmInst(Opcode::Enum O, int64_t Imm) : Arg(Imm), Op(O) {}
  WasmInst(Opcode::Enum O, uint64_t Imm) : Arg(Imm), Op(O) {}
  WasmInst(Opcode::Enum O, uint32_t Imm)
      : Arg(static_cast<uint64_t>(Imm)), Op(O) {}
  WasmInst(Opcode::Enum O, float Imm) : Arg(Imm), Op(O) {}
  WasmInst(Opcode::Enum O, double Imm) : Arg(Imm), Op(O) {}

  template <typename T>
  WasmInst(Opcode::Enum O, T &&A) : Arg(std::forward<T>(A)), Op(O) {}

  static WasmInst createBlock(ValType Ty) {
    return {Opcode::Block, BlockTypeArg{Ty}};
  }
  static WasmInst createLoop(ValType Ty) {
    return {Opcode::Loop, BlockTypeArg{Ty}};
  }
  static WasmInst createIfElse(ValType Ty) {
    return {Opcode::If, BlockTypeArg{Ty}};
  }

#ifdef WATEVER_LOGGING
  [[nodiscard]] std::string getString(
      const llvm::SmallVector<llvm::SmallVector<uint32_t>, 0> &BranchTables)
      const;
#endif

  void write(llvm::raw_ostream &OS, Relocation &Reloc,
             llvm::DenseMap<uint32_t, uint32_t> &LocalMap,
             const llvm::SmallVector<llvm::SmallVector<uint32_t>, 0>
                 &BranchTables) const;
};

class WasmActions {
public:
  llvm::SmallVector<WasmInst, 8> Insts;
  llvm::SmallVector<llvm::SmallVector<uint32_t>, 0> BranchTables;

  void appendBranchTable(llvm::SmallVector<uint32_t> Targets,
                         uint32_t Default) {
    auto TableIdx = BranchTables.size();
    BranchTables.push_back(std::move(Targets));
    Insts.emplace_back(
        Opcode::BrTable,
        BranchTableArg{.TableIdx = TableIdx, .DefaultTarget = Default});
  }
};

} // namespace watever
