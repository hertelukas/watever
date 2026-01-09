#include "watever/color.hpp"
#include "watever/utils.hpp"
#include <algorithm>
#include <llvm/IR/Argument.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/User.h>
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

bool FunctionColorer::needsColor(llvm::Value *) { return true; }
void FunctionColorer::color(llvm::BasicBlock *) {}

void FunctionColorer::run() {
  computeLiveSets();
  WATEVER_LOG_DBG("Calculated liveness for {}", Source.getName().str());
#ifdef WATEVER_LOGGING
  dumpLiveness();
#endif
  color(&Source.getEntryBlock());
}
