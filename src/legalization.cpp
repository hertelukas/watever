#include "watever/legalization.hpp"
#include "watever/utils.hpp"
#include <llvm/ADT/STLExtras.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>

using namespace watever;

llvm::Function *LegalizationPass::createLegalFunction(llvm::Module &Mod,
                                                      llvm::Function *OldFunc) {
  llvm::Function *Fn = llvm::Function::Create(
      createLegalFunctionType(OldFunc->getFunctionType()),
      OldFunc->getLinkage(), OldFunc->getName(), Mod);

  for (auto [OldArg, NewArg] : llvm::zip_equal(OldFunc->args(), Fn->args())) {
    NewArg.setName(OldArg.getName());
  }
  return Fn;
}

llvm::FunctionType *
LegalizationPass::createLegalFunctionType(llvm::FunctionType *OldFuncTy) {
  llvm::Type *ResultTy = getLegalType(OldFuncTy->getReturnType());
  llvm::SmallVector<llvm::Type *> Params;

  for (auto *Param : OldFuncTy->params()) {
    Params.push_back(getLegalType(Param));
  }

  return llvm::FunctionType::get(ResultTy, Params, OldFuncTy->isVarArg());
}

llvm::Type *LegalizationPass::getLegalType(llvm::Type *Ty) {
  if (Ty->isIntegerTy()) {
    if (Ty->getIntegerBitWidth() <= 32) {
      return llvm::Type::getInt32Ty(Ty->getContext());
    }
    if (Ty->getIntegerBitWidth() <= 64) {
      return llvm::Type::getInt64Ty(Ty->getContext());
    }
  }

  if (Ty->isFloatingPointTy()) {
    if (Ty->isFloatTy() || Ty->isDoubleTy()) {
      return Ty;
    }
  }
  WATEVER_UNIMPLEMENTED("Unsupported type {}", llvmToString(*Ty));
}

llvm::PreservedAnalyses LegalizationPass::run(llvm::Module &Mod,
                                              llvm::ModuleAnalysisManager &) {

  llvm::SmallVector<llvm::Function *> FuncsToLegalize;
  for (auto &F : Mod) {
    // TODO handle declarations
    if (F.isDeclaration()) {
      continue;
    }
    FuncsToLegalize.push_back(&F);
  }

  for (auto *F : FuncsToLegalize) {
    WATEVER_LOG_DBG("Legalizing {}", F->getName().str());
    auto *NewFunc = LegalizationPass::createLegalFunction(Mod, F);
    llvm::IRBuilder<> Builder(Mod.getContext());
    FunctionLegalizer FL{F, NewFunc, Builder};
    FL.visit(F);
    FL.NewFunc->takeName(F);
    WATEVER_LOG_DBG("Legalized Function:\n {}", llvmToString(*FL.NewFunc));
    F->eraseFromParent();
  }

  return llvm::PreservedAnalyses::none();
}
