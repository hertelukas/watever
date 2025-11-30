#ifndef IR_H
#define IR_H

#include "watever/opcode.h"
#include <llvm/ADT/SmallVector.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/IR/Analysis.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <memory>
#include <vector>

namespace watever {

struct WasmInstruction {
  Opcode Op;
  llvm::SmallVector<WasmInstruction *, 4> Operands;
};

class Wasm {
public:
  virtual ~Wasm() = default;
};

class WasmBlock : public Wasm {
public:
  std::unique_ptr<Wasm> InnerWasm;

  explicit WasmBlock(std::unique_ptr<Wasm> Inner)
      : InnerWasm(std::move(Inner)) {};
};

class WasmLoop : public Wasm {
public:
  std::unique_ptr<Wasm> InnerWasm;

  explicit WasmLoop(std::unique_ptr<Wasm> Inner)
      : InnerWasm(std::move(Inner)) {};
};

class WasmIf : public Wasm {
public:
  WasmInstruction Condition;
  std::unique_ptr<Wasm> TrueWasm;
  std::unique_ptr<Wasm> FalseWasm;
};

class WasmBr : public Wasm {
  int Nesting;
};

class WasmReturn : public Wasm {};

class WasmActions : public Wasm {
  std::vector<std::unique_ptr<WasmInstruction>> Roots;
};

class WasmSeq : public Wasm {
  std::vector<std::unique_ptr<Wasm>> Flow;
};

class Function {};
class Module {
public:
  llvm::SmallVector<Function, 4> Functions;
};

class FunctionLowering {
  enum class BlockType {
    IfThenElse,
    Loop,
    Block,
  };
  class ContainingSyntax {
    BlockType BT;
    llvm::BasicBlock *Label;

    ContainingSyntax(BlockType BT, llvm::BasicBlock *Label)
        : BT(BT), Label(Label) {}

  public:
    static ContainingSyntax createIf() {
      return {BlockType::IfThenElse, nullptr};
    }
    static ContainingSyntax createLoop(llvm::BasicBlock *Header) {
      return {BlockType::Loop, Header};
    }
    static ContainingSyntax createBlock(llvm::BasicBlock *Follow) {
      return {BlockType::Block, Follow};
    }
  };

  using Context = llvm::SmallVector<ContainingSyntax, 8>;

  llvm::DominatorTree &DT;
  llvm::LoopInfo &LI;

  std::unique_ptr<Wasm> doBranch(llvm::BasicBlock *TrueBlock,
                                 llvm::BasicBlock *FalseBlock, Context &Ctx);

  // TODO MergeChildren needs better type
  std::unique_ptr<Wasm>
  nodeWithin(llvm::BasicBlock *Parent,
             llvm::SmallVector<llvm::BasicBlock *> MergeChildren, Context &Ctx);

  std::unique_ptr<Wasm> doTree(llvm::BasicBlock *Root, Context &Ctx);

  void getMergeChildren(llvm::BasicBlock *R,
                        llvm::SmallVectorImpl<llvm::BasicBlock *> &Result);

  static bool isMergeNode(const llvm::BasicBlock *BB) {
    return !llvm::pred_empty(BB) && !BB->getSinglePredecessor();
  }

public:
  FunctionLowering(llvm::DominatorTree &DT, llvm::LoopInfo &LI)
      : DT(DT), LI(LI) {}

  std::unique_ptr<Wasm> lower() {
    Context Ctx;
    return doTree(DT.getRoot(), Ctx);
  }
};

class ModuleLowering {

public:
  Module convert(llvm::Module &Mod, llvm::FunctionAnalysisManager &FAM);
};

} // namespace watever

#endif /* IR_H */
