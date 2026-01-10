#include "watever/color.hpp"
#include "watever/symbol.hpp"
#include "watever/type.hpp"
#include "watever/utils.hpp"
#include <algorithm>
#include <llvm/ADT/DenseSet.h>
#include <llvm/IR/Argument.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/User.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/raw_ostream.h>

using namespace watever;

bool FunctionColorer::isDefinedInBlock(llvm::Value *Val, llvm::BasicBlock *BB) {
  // PHIs and arguments are already live-in to their respective blocks
  if (llvm::isa<llvm::PHINode>(Val) || llvm::isa<llvm::Argument>(Val)) {
    return false;
  }

  if (auto *Inst = llvm::dyn_cast<llvm::Instruction>(Val)) {
    return Inst->getParent() == BB;
  }

  return false;
}

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
  std::string Buffer;
  llvm::raw_string_ostream OS(Buffer);

  auto NameComparison = [](const llvm::Value *A, const llvm::Value *B) {
    if (A->hasName() && B->hasName())
      return A->getName() < B->getName();
    // Backup, if e.g., %1
    std::string StrA, StrB;
    llvm::raw_string_ostream OSA(StrA), OSB(StrB);
    A->printAsOperand(OSA, false);
    B->printAsOperand(OSB, false);
    return OSA.str() < OSA.str();
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

  if (I.getType()->isVoidTy()) {
    return false;
  }

  if (I.mayHaveSideEffects() && I.getNumUses()) {
    return true;
  }

  auto *Parent = I.getParent();
  for (auto *U : I.users()) {
    if (auto *UserInst = llvm::dyn_cast<llvm::Instruction>(U)) {
      if (UserInst->getParent() != Parent) {
        return true;
      }
    }
  }

  return false;
}

void FunctionColorer::color(llvm::BasicBlock *BB) {
  WATEVER_LOG_TRACE("Coloring block {}", getBlockName(BB));
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

  for (auto &Inst : *BB) {
    // Remove last uses
    for (auto &_ : Inst.operands()) {
      // Check if last use
      // TODO this is not that easy to decide, as we reschedule instructions
      // based on the tree build by instructions with side effects. A safe
      // option would be to mark a mapping no-longer used at the end of this
      // BB - and defer any decision until then, which is happening anyway, as
      // these will never be live-in anymore.
    }
    if (needsColor(Inst)) {
      auto LocalType = fromLLVMType(Inst.getType(), BB->getDataLayout());

      auto *Local = getFreeLocal(LocalType, Assigned);
      Target->LocalMapping[&Inst] = Local;
      Assigned.insert(Local);

      WATEVER_LOG_TRACE("Mapping {} to local {}", Inst.getNameOrAsOperand(),
                        Target->LocalMapping[&Inst]->Index);
    }
  }

  // Handle imm dominated children
  for (auto *Child : *DT.getNode(BB)) {
    color(Child->getBlock());
  }
}

void FunctionColorer::run() {
  computeLiveSets();
  WATEVER_LOG_DBG("Calculated liveness for {}", Source.getName().str());
#ifdef WATEVER_LOGGING
  dumpLiveness();
#endif
  color(&Source.getEntryBlock());
}
