#pragma once

#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/Analysis.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/InstVisitor.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Type.h>

#include "watever/utils.hpp"

namespace watever {

class FunctionLegalizer : public llvm::InstVisitor<FunctionLegalizer> {

  // Maps Old Value -> New Legal Value
  llvm::DenseMap<llvm::Value *, llvm::Value *> ValueMap{};
  llvm::IRBuilder<> &Builder;

public:
  llvm::Function *NewFunc;
  FunctionLegalizer(llvm::Function *OldFunc, llvm::Function *NewFunc,
                    llvm::IRBuilder<> &B)
      : Builder(B), NewFunc(NewFunc) {

    // TODO this is generally not correct, e.g., vectors are returned through an
    // argument
    for (auto [OldArg, NewArg] :
         llvm::zip_equal(OldFunc->args(), NewFunc->args())) {
      ValueMap[&OldArg] = &NewArg;
    }

    for (auto &OldBB : *OldFunc) {
      llvm::BasicBlock *NewBB = llvm::BasicBlock::Create(
          NewFunc->getContext(), OldBB.getName(), NewFunc);
      ValueMap[&OldBB] = NewBB;
    }
  }

  void visitInstruction(llvm::Instruction &I) {
    // TODO set to UNIMPLEMENTED
    WATEVER_TODO("{} not (yet) supported", I.getOpcodeName());
  }
};

class LegalizationPass : public llvm::PassInfoMixin<LegalizationPass> {

  static llvm::Function *createLegalFunction(llvm::Module &M,
                                             llvm::Function *OldFunc);

  static llvm::FunctionType *
  createLegalFunctionType(llvm::FunctionType *OldFuncTy);

public:
  static llvm::Type *getLegalType(llvm::Type *Ty);
  llvm::PreservedAnalyses run(llvm::Module &M,
                              llvm::ModuleAnalysisManager &MAM);
};
} // namespace watever
