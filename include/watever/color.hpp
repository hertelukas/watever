#pragma once

#include "symbol.hpp"
#include "watever/type.hpp"
#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/DenseSet.h>
#include <llvm/ADT/SmallPtrSet.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Value.h>

namespace watever {
class FunctionColorer {
  llvm::Function &Source;
  DefinedFunc *Target;

  llvm::DominatorTree &DT;

  llvm::DenseMap<llvm::BasicBlock *, llvm::SmallVector<llvm::Value *>> LiveIn;
  llvm::DenseMap<llvm::BasicBlock *, llvm::SmallVector<llvm::Value *>> LiveOut;
  llvm::DenseMap<llvm::AllocaInst *, llvm::BasicBlock *> PromotedAIStartBlocks;
  llvm::DenseMap<llvm::BasicBlock *, llvm::SmallVector<llvm::AllocaInst *>>
      AllocsStartingAt;

  /// Checks whether all uses of \p AI can be realized with set/get operations on
  /// locals. Returns the NCD block of all load/stores \p AI is used in, or
  /// nullptr, if promotion is not possible.
  llvm::BasicBlock *canBePromoted(llvm::AllocaInst *AI);
  bool isDefinedInBlock(llvm::Value *Val, llvm::BasicBlock *BB);
  void upAndMark(llvm::BasicBlock *BB, llvm::Value *Val);
  // This uses Florian Bradner et al, Computing Liveness Sets for SSA-Form
  // Programs, algorithm 6
  void computeLiveSets();
#ifdef WATEVER_LOGGING
  void dumpLiveness();
#endif

  Local *getFreeLocal(ValType Type, const llvm::DenseSet<Local *> &Assigned);
  bool needsColor(llvm::Instruction &I);
  void color(llvm::BasicBlock *BB);

  /// Returns true, iff the \p A is live in a block in which \p B is also live
  bool interfere(llvm::Instruction *A, llvm::Instruction *B);

public:
  FunctionColorer(llvm::Function &F, DefinedFunc *T, llvm::DominatorTree &DT)
      : Source(F), Target(T), DT(DT) {}
  /// Tries to color Target by creating as few Target->Locals as possible. To
  /// get the mapped information, the targets LocalMapping is filled.
  void run();
};
} // namespace watever
