#pragma once

#include "symbol.hpp"
#include "watever/type.hpp"
#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/DenseSet.h>
#include <llvm/ADT/EquivalenceClasses.h>
#include <llvm/ADT/SmallPtrSet.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Value.h>

namespace watever {

struct AffinityEdge {
  llvm::Value *Source;
  llvm::Value *Target;
  uint64_t Weight;

  explicit AffinityEdge(llvm::Value *S, llvm::Value *T, uint64_t W)
      : Source(S), Target(T), Weight(W) {}

  auto operator<=>(const AffinityEdge &Other) const {
    return Weight <=> Other.Weight;
  }
};

class FunctionColorer {
  llvm::Function &Source;
  DefinedFunc *Target;

  llvm::DominatorTree &DT;
  llvm::FunctionAnalysisManager &FAM;

  llvm::DenseMap<llvm::BasicBlock *, llvm::SmallVector<llvm::Value *>> LiveIn;
  llvm::DenseMap<llvm::BasicBlock *, llvm::SmallVector<llvm::Value *>> LiveOut;
  llvm::DenseMap<llvm::AllocaInst *, llvm::BasicBlock *> PromotedAIStartBlocks;
  llvm::DenseMap<llvm::BasicBlock *, llvm::SmallVector<llvm::AllocaInst *>>
      AllocsStartingAt;

  // Maps for which values the lifetime ends at a given instruction
  llvm::DenseMap<llvm::Instruction *, llvm::SmallPtrSet<llvm::Value *, 8>>
      LastUses;

  // TODO maybe use a better data structure for cache locality
  llvm::DenseMap<llvm::BasicBlock *,
                 llvm::DenseMap<llvm::Value *, llvm::Instruction *>>
      DyingAt;

  llvm::EquivalenceClasses<llvm::Value *> Chunks;
  llvm::SmallVector<AffinityEdge> Affinities;

  /// Checks whether all uses of \p AI can be realized with set/get operations
  /// on locals. Returns the NCD block of all load/stores \p AI is used in, or
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

  void computeLastUses(llvm::BasicBlock *BB);

  uint32_t getFreeLocal(ValType Type, const llvm::DenseSet<uint32_t> &Assigned);
  bool needsColor(llvm::Instruction &I);
  void color(llvm::BasicBlock *BB);

  /// Returns true, iff the \p A is live in a block in which \p B is also live
  bool interfere(llvm::Value *A, llvm::Value *B);
  void getInterferenceNeighbors(llvm::Value *Val,
                                llvm::DenseSet<llvm::Value *> &Neighbors);
  void findInterference(llvm::BasicBlock *BB, llvm::Value *Val,
                        llvm::DenseSet<llvm::Value *> &Neighbors,
                        llvm::DenseSet<llvm::BasicBlock *> &Visited);

  void computeAffinityGraph();
  void recolorChunk(const llvm::EquivalenceClasses<llvm::Value *>::ECValue *EC);
  void buildChunks();
  void coalesce();

public:
  FunctionColorer(llvm::Function &F, DefinedFunc *T, llvm::DominatorTree &DT,
                  llvm::FunctionAnalysisManager &FAM)
      : Source(F), Target(T), DT(DT), FAM(FAM) {}
  /// Tries to color Target by creating as few Target->Locals as possible. To
  /// get the mapped information, the targets LocalMapping is filled.
  void run();
};
} // namespace watever
