#include "watever/color.hpp"
#include "watever/symbol.hpp"
#include "watever/type.hpp"
#include "watever/utils.hpp"
#include <algorithm>
#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/DenseSet.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/Analysis/BlockFrequencyInfo.h>
#include <llvm/Analysis/BranchProbabilityInfo.h>
#include <llvm/IR/Argument.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/User.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/raw_ostream.h>

using namespace watever;

llvm::BasicBlock *FunctionColorer::canBePromoted(llvm::AllocaInst *AI) {
  if (!AI->isStaticAlloca())
    return nullptr;

  // Only promote legal Wasm types
  auto *Ty = AI->getAllocatedType();
  if (!Ty->isIntegerTy(32) && !Ty->isIntegerTy(64) && !Ty->isPointerTy() &&
      !Ty->isDoubleTy() && !Ty->isFloatTy()) {
    return nullptr;
  }

  // We need a common dominator to ensure that all uses are dominated by their
  // definition. One could also treat stores as definitions; this however would
  // introduce new PHI nodes, as some paths might then use a different color for
  // the same underlying memory location.
  // Note that the current solution also violates SSA, as it allows us to assign
  // to locals multiple times.
  llvm::BasicBlock *CommonDom = nullptr;

  for (llvm::Use &Use : AI->uses()) {
    auto *UserInst = llvm::dyn_cast<llvm::Instruction>(Use.getUser());
    if (!UserInst)
      return nullptr;
    // The user must use AI as pointer (not as argument)
    if (auto *LI = llvm::dyn_cast<llvm::LoadInst>(UserInst)) {
      if (LI->getPointerOperand() != AI || LI->isVolatile())
        return nullptr;
    } else if (auto *SI = llvm::dyn_cast<llvm::StoreInst>(UserInst)) {
      if (SI->getPointerOperand() != AI || SI->isVolatile())
        return nullptr;
    } else {
      return nullptr;
    }

    // Nearest common dominator
    llvm::BasicBlock *UseBlock = nullptr;
    if (auto *Phi = llvm::dyn_cast<llvm::PHINode>(UserInst)) {
      UseBlock = Phi->getIncomingBlock(Use);
    } else {
      UseBlock = UserInst->getParent();
    }

    if (!CommonDom) {
      CommonDom = UseBlock;
    } else {
      CommonDom = DT.findNearestCommonDominator(CommonDom, UseBlock);
    }
  }
  assert(CommonDom && "no start block of promoted AI found");
  WATEVER_LOG_TRACE("Promoting {} (StartBlock: {})", AI->getNameOrAsOperand(),
                    getBlockName(CommonDom));
  return CommonDom;
}

bool FunctionColorer::isDefinedInBlock(llvm::Value *Val, llvm::BasicBlock *BB) {
  // PHIs and arguments are already live-in to their respective blocks
  if (llvm::isa<llvm::PHINode>(Val) || llvm::isa<llvm::Argument>(Val)) {
    return false;
  }

  if (auto *AI = llvm::dyn_cast<llvm::AllocaInst>(Val)) {
    if (auto It = PromotedAIStartBlocks.find(AI);
        It != PromotedAIStartBlocks.end()) {
      return It->second == BB;
    }
  }

  if (auto *Inst = llvm::dyn_cast<llvm::Instruction>(Val)) {
    return Inst->getParent() == BB;
  }

  return false;
}

// If \p Val is used in \p BB, marks \p Val as live-in/live-out on all paths
// from \p def(Val) to \p BB.
void FunctionColorer::upAndMark(llvm::BasicBlock *BB, llvm::Value *Val) {
  if (isDefinedInBlock(Val, BB)) {
    return;
  }
  // Propagation done, stop
  if (!LiveIn[BB].empty() && LiveIn[BB].back() == Val) {
    return;
  }

  LiveIn[BB].push_back(Val);

  // Do not propagate phi definitions
  if (auto *Phi = llvm::dyn_cast<llvm::PHINode>(Val)) {
    if (Phi->getParent() == BB)
      return;
  }

  for (auto *Pred : llvm::predecessors(BB)) {
    if (LiveOut[Pred].empty() || LiveOut[Pred].back() != Val) {
      LiveOut[Pred].push_back(Val);
    }
    upAndMark(Pred, Val);
  }
}

void FunctionColorer::computeLiveSets() {
  for (auto &Inst : Source.getEntryBlock()) {
    if (auto *AI = llvm::dyn_cast<llvm::AllocaInst>(&Inst)) {
      if (auto *StartBlock = canBePromoted(AI)) {
        PromotedAIStartBlocks[AI] = StartBlock;
        AllocsStartingAt[StartBlock].push_back(AI);
      }
    }
  }

  auto HandleVariable = [&](llvm::Value &V) {
    for (llvm::Use &U : V.uses()) {
      auto *UserInst = llvm::cast<llvm::Instruction>(U.getUser());
      llvm::BasicBlock *UserParentBlock = nullptr;

      if (auto *Phi = llvm::dyn_cast<llvm::PHINode>(UserInst)) {
        UserParentBlock = Phi->getIncomingBlock(U);
        LiveOut[UserParentBlock].push_back(&V);
      } else {
        UserParentBlock = UserInst->getParent();
      }
      upAndMark(UserParentBlock, &V);
    }
  };

  for (auto &Arg : Source.args()) {
    HandleVariable(Arg);
  }

  for (auto &I : llvm::instructions(Source)) {
    HandleVariable(I);
  }
}

#ifdef WATEVER_LOGGING
void FunctionColorer::dumpLiveness() {
  if (!spdlog::should_log(spdlog::level::trace)) {
    return;
  }
  std::string Buffer;
  llvm::raw_string_ostream OS(Buffer);

  auto NameComparison = [](const llvm::Value *A, const llvm::Value *B) {
    if (A->hasName() && B->hasName())
      return A->getName() < B->getName();
    return A->getNameOrAsOperand() < B->getNameOrAsOperand();
  };

  for (auto &BB : Source) {
    std::sort(LiveIn[&BB].begin(), LiveIn[&BB].end(), NameComparison);
    std::sort(LiveOut[&BB].begin(), LiveOut[&BB].end(), NameComparison);

    OS << "Liveness for block " << getBlockName(&BB) << "\n";

    OS << "Live In {\n";
    for (auto *V : LiveIn[&BB]) {
      OS << "  ";
      V->printAsOperand(OS, false);
      OS << "\n";
    }
    OS << "}\n";

    OS << "Live Out {\n";
    for (auto *V : LiveOut[&BB]) {
      OS << "  ";
      V->printAsOperand(OS, false);
      OS << "\n";
    }
    OS << "}\n";
  }

  WATEVER_LOG_TRACE("\n{}", OS.str());
}
#endif

// This is not optimal. It sets the last use as the root of the last tree using
// each value. This is not optimal, e.g., here:
// loop:
// %b = phi i32 [ %a, %entry ], [ %c, %loop ]
// %c = add i32 %b, 1
// %cond = icmp eq i32 %n, %c
// br i1 %cond, label %loop, label %exit
//
// %c could safely reuse the local used for %b. However, the local for %b is
// only available after the branch (the root of this evaluation tree).
void FunctionColorer::computeLastUses(llvm::BasicBlock *BB) {
  // All these values' lifetime end during this BB
  llvm::DenseSet<llvm::Value *> MustDie(LiveIn[BB].begin(), LiveIn[BB].end());
  for (auto &I : *BB) {
    if (!I.getType()->isVoidTy()) {
      MustDie.insert(&I);
    }
  }
  for (auto *V : LiveOut[BB])
    MustDie.erase(V);

  // Forward scan finding latest user
  llvm::SmallVector<llvm::Value *> WorkList;
  // If we have visited a value in the current tree, we have already updated
  // what must die
  llvm::DenseSet<llvm::Value *> VisitedInCurrentTree;

  llvm::DenseSet<llvm::Value *> Roots;

  for (auto &Inst : *BB) {
    if (Inst.mayHaveSideEffects() || Inst.isTerminator() ||
        Target->hasExternalUser(&Inst, BB)) {
      VisitedInCurrentTree.clear();
      WorkList.push_back(&Inst);

      while (!WorkList.empty()) {
        auto *Next = WorkList.pop_back_val();

        if (!VisitedInCurrentTree.insert(Next).second)
          continue;

        if (MustDie.contains(Next)) {
          DyingAt[BB][Next] = &Inst;
        }

        if (Roots.contains(Next))
          continue;

        if (auto *I = llvm::dyn_cast<llvm::Instruction>(Next)) {
          if (I->getParent() == BB) {
            for (llvm::Value *Op : I->operands()) {
              WorkList.push_back(Op);
            }
          }
        }
      }
      Roots.insert(&Inst);
    }
  }

  for (auto &[Val, Root] : DyingAt[BB]) {
    WATEVER_LOG_TRACE("{} is last use of {}", llvmToString(*Root),
                      Val->getNameOrAsOperand());
    LastUses[Root].insert(Val);
  }
}

Local *FunctionColorer::getFreeLocal(ValType Type,
                                     const llvm::DenseSet<Local *> &Assigned) {
  for (auto &L : Target->Arguments[Type]) {
    if (!Assigned.contains(L)) {
      return L;
    }
  }

  for (auto &L : Target->Locals[Type]) {
    if (!Assigned.contains(L)) {
      return L;
    }
  }

  // Failed to find unused local
  return Target->getNewLocal(Type);
}

bool FunctionColorer::needsColor(llvm::Instruction &I) {
  if (llvm::isa<llvm::PHINode>(I)) {
    return true;
  }

  if (auto *AI = llvm::dyn_cast<llvm::AllocaInst>(&I)) {
    if (PromotedAIStartBlocks.contains(AI)) {
      return false;
    }
  }

  if (I.getType()->isVoidTy()) {
    return false;
  }

  if (I.mayHaveSideEffects() && I.getNumUses()) {
    return true;
  }

  auto *Parent = I.getParent();
  for (auto *U : I.users()) {
    if (auto *UserInst = llvm::dyn_cast<llvm::Instruction>(U)) {
      if (llvm::isa<llvm::PHINode>(UserInst)) {
        return true;
      }
      if (UserInst->getParent() != Parent) {
        return true;
      }
    }
  }
  return false;
}

void FunctionColorer::color(llvm::BasicBlock *BB) {
  WATEVER_LOG_TRACE("Coloring block {}", getBlockName(BB));
  computeLastUses(BB);
  // TODO maybe it is cheaper to keep track of unassigned?
  llvm::DenseSet<Local *> Assigned;

  for (auto *Val : LiveIn[BB]) {
    // Phi nodes are technically live-in but ignored by Hack
    if (auto *Phi = llvm::dyn_cast<llvm::PHINode>(Val)) {
      if (Phi->getParent() == BB)
        continue;
    }

    // There might be values live-in which do not have a local
    if (Target->LocalMapping.contains(Val)) {
      Assigned.insert(Target->LocalMapping[Val]);
    } else {
      // This is generally okay, however, it will force the lowering to use a
      // completely new local and should therefore not be the default. Can be
      // prevented by handling this kind of value in needsColor
      WATEVER_LOG_INFO("{} is live-in but has no local assigned to it",
                       Val->getNameOrAsOperand());
    }
  }

  auto colorPromotedAlloca = [&](llvm::AllocaInst *AI) {
    if (Target->LocalMapping.contains(AI)) {
      return;
    }
    auto LocalType = fromLLVMType(AI->getAllocatedType(), BB->getDataLayout());
    auto *Local = getFreeLocal(LocalType, Assigned);

    Target->LocalMapping[AI] = Local;
    Target->PromotedAllocas[AI] = Local;
    Assigned.insert(Local);

    WATEVER_LOG_TRACE("Mapping promoted {} to local {}",
                      AI->getNameOrAsOperand(), Local->Index);
  };

  for (auto &Inst : *BB) {
    // Remove last uses
    for (auto *Val : LastUses[&Inst]) {
      if (auto It = Target->LocalMapping.find(Val);
          It != Target->LocalMapping.end()) {
        if (Assigned.erase(It->second)) {
          WATEVER_LOG_TRACE("Unmapping {}", Val->getNameOrAsOperand());
        }
      }
    }
    // This might be a load/store with a promoted alloca pointer, which has not
    // been assigned a local yet.
    if (auto *LI = llvm::dyn_cast<llvm::LoadInst>(&Inst)) {
      if (auto *AI =
              llvm::dyn_cast<llvm::AllocaInst>(LI->getPointerOperand())) {
        if (PromotedAIStartBlocks.contains(AI)) {
          colorPromotedAlloca(AI);
          continue;
        }
      }
    } else if (auto *SI = llvm::dyn_cast<llvm::StoreInst>(&Inst)) {
      if (auto *AI =
              llvm::dyn_cast<llvm::AllocaInst>(SI->getPointerOperand())) {
        if (PromotedAIStartBlocks.contains(AI)) {
          colorPromotedAlloca(AI);
          continue;
        }
      }
    }
    // If not, check if we need a color for the result
    if (needsColor(Inst)) {
      auto LocalType = fromLLVMType(Inst.getType(), BB->getDataLayout());

      auto *Local = getFreeLocal(LocalType, Assigned);
      Target->LocalMapping[&Inst] = Local;
      Assigned.insert(Local);

      WATEVER_LOG_TRACE("Mapping {} to local {}", Inst.getNameOrAsOperand(),
                        Target->LocalMapping[&Inst]->Index);
    }
  }

  // There might be alloca's who's lifetime start now, even though they are
  // never used in this block, as this might be the NCD.
  if (auto It = AllocsStartingAt.find(BB); It != AllocsStartingAt.end()) {
    for (auto *AI : It->second) {
      colorPromotedAlloca(AI);
    }
  }

  // Handle imm dominated children
  for (auto *Child : *DT.getNode(BB)) {
    color(Child->getBlock());
  }
}

bool FunctionColorer::interfere(llvm::Value *A, llvm::Value *B) {
  auto getParent = [&](llvm::Value *Val) {
    if (auto *Inst = llvm::dyn_cast<llvm::Instruction>(Val)) {
      return Inst->getParent();
    }
    if (llvm::isa<llvm::Argument>(Val)) {
      return &Source.getEntryBlock();
    }
    WATEVER_UNREACHABLE("cannot check interference for {}", llvmToString(*Val));
  };

  llvm::BasicBlock *AParent = getParent(A);
  llvm::BasicBlock *BParent = getParent(B);
  llvm::Value *First = nullptr;
  llvm::Value *Second = nullptr;

  bool ABeforeB = false;

  if (AParent == BParent) {
    auto *IA = llvm::dyn_cast<llvm::Instruction>(A);
    auto *IB = llvm::dyn_cast<llvm::Instruction>(B);

    if (llvm::isa<llvm::Argument>(A))
      ABeforeB = true;
    else if (llvm::isa<llvm::Argument>(B))
      ABeforeB = false;
    else
      ABeforeB = DT.dominates(IA, IB);
  } else {
    if (DT.dominates(AParent, BParent)) {
      ABeforeB = true;
    } else if (DT.dominates(BParent, AParent)) {
      ABeforeB = false;
    } else {
      return false;
    }
  }

  First = ABeforeB ? A : B;
  Second = ABeforeB ? B : A;

  // If the value of the first is live out at the block defining second, we
  // interfere
  for (llvm::Value *LiveOut : LiveOut[getParent(Second)]) {
    if (LiveOut == First) {
      return true;
    }
  }

  // At this point, first might be live in the block where second is defined,
  // but not at its end. They interfere if the last using root of first comes
  // after the definition of second.
  if (auto *DeathFirst = DyingAt[getParent(Second)].lookup(First)) {
    if (DT.dominates(Second, DeathFirst)) {
      return true;
    }
  }

  return false;
}

// We will only ever have moves between phi nodes, so only iterate over them
// when building the affinity graph
void FunctionColorer::computeAffinityGraph() {
  auto &BFI = FAM.getResult<llvm::BlockFrequencyAnalysis>(Source);
  auto &BPI = FAM.getResult<llvm::BranchProbabilityAnalysis>(Source);
  for (auto &BB : Source) {
    for (auto &Phi : BB.phis()) {
      for (size_t I = 0; I < Phi.getNumIncomingValues(); ++I) {
        llvm::Value *IncomingValue = Phi.getIncomingValue(I);
        llvm::BasicBlock *IncomingBB = Phi.getIncomingBlock(I);
        if (llvm::isa<llvm::Instruction>(IncomingValue) ||
            llvm::isa<llvm::Argument>(IncomingValue)) {
          uint64_t BlockFrequency = BFI.getBlockFreq(IncomingBB).getFrequency();
          uint64_t Weight =
              BPI.getEdgeProbability(IncomingBB, &BB).scale(BlockFrequency);
          Affinities.emplace_back(IncomingValue, &Phi, Weight);
        }
      }
    }
  }
  std::ranges::sort(Affinities, std::greater{});

#ifdef WATEVER_LOGGING
  std::string Buffer;
  llvm::raw_string_ostream OS(Buffer);

  OS << "Affinities:\n";

  for (const auto &Edge : Affinities) {
    OS << "From " << Edge.Source->getNameOrAsOperand() << " to "
       << Edge.Target->getNameOrAsOperand() << ": " << Edge.Weight << "\n";
  }

  WATEVER_LOG_TRACE("\n{}", OS.str());
#endif
}

void FunctionColorer::coalesce() {
  computeAffinityGraph();
  buildChunks();
}

void FunctionColorer::buildChunks() {
  // TODO this could be done during coloring
  for (auto &Arg : Source.args()) {
    Chunks.insert(&Arg);
  }
  for (auto &Inst : llvm::instructions(Source)) {
    if (needsColor(Inst)) {
      Chunks.insert(&Inst);
    }
  }

  for (const auto &Edge : Affinities) {
    if (Chunks.isEquivalent(Edge.Source, Edge.Target)) {
      continue;
    }
    auto interferes = [&]() {
      for (auto *MemberSource : Chunks.members(Edge.Source)) {
        for (auto *MemberTarget : Chunks.members(Edge.Target)) {
          if (interfere(MemberSource, MemberTarget)) {
            WATEVER_LOG_TRACE(
                "Cannot merge chunks {} and {}. {} and {} interfere",
                Edge.Source->getNameOrAsOperand(),
                Edge.Target->getNameOrAsOperand(),
                MemberSource->getNameOrAsOperand(),
                MemberTarget->getNameOrAsOperand());
            return true;
          }
        }
      }
      return false;
    };

    if (!interferes()) {
      WATEVER_LOG_TRACE("Merging chunks {} and {}",
                        Edge.Source->getNameOrAsOperand(),
                        Edge.Target->getNameOrAsOperand());
      Chunks.unionSets(Edge.Source, Edge.Target);
    }
  }
}

void FunctionColorer::run() {
  computeLiveSets();
  WATEVER_LOG_DBG("Calculated liveness for {}", Source.getName().str());
#ifdef WATEVER_LOGGING
  dumpLiveness();
#endif
  color(&Source.getEntryBlock());
  coalesce();
}
