#pragma once

#include <llvm/ADT/StringRef.h>
#include <llvm/IR/Analysis.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/InstVisitor.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Type.h>

#include "watever/utils.hpp"

namespace watever {

class LegalizationPass : public llvm::PassInfoMixin<LegalizationPass>,
                         public llvm::InstVisitor<LegalizationPass> {

  llvm::Type *Int8Ty;
  llvm::Type *Int16Ty;
  llvm::Type *Int32Ty;
  llvm::Type *Int64Ty;
  llvm::Type *PtrTy;
  llvm::Type *IntPtrTy;

  template <typename LegalOpFn, typename ExpandFn>
  bool legalizeIntegerBinaryOp(llvm::BinaryOperator &BO,
                               LegalOpFn CreateLegalOp, ExpandFn HandleExpand) {
    auto *InstType = BO.getType();

    if (!InstType->isIntegerTy()) {
      WATEVER_TODO("expanding vector {} not supported", BO.getOpcodeName());
      return false;
    }

    unsigned Width = InstType->getIntegerBitWidth();

    if (Width == 32 || Width == 64) {
      return true;
    }

    llvm::IRBuilder<> Builder(&BO);

    if (Width > 64) {
      return HandleExpand(Builder, BO);
    }

    auto *TargetTy = Width < 32 ? Int32Ty : Int64Ty;
    llvm::Value *ExtA = Builder.CreateZExt(BO.getOperand(0), TargetTy);
    llvm::Value *ExtB = Builder.CreateZExt(BO.getOperand(1), TargetTy);
    llvm::Value *LegalOp = CreateLegalOp(Builder, ExtA, ExtB, Width);
    llvm::Value *TruncResult = Builder.CreateTrunc(LegalOp, InstType);
    BO.replaceAllUsesWith(TruncResult);
    BO.eraseFromParent();

    return true;
  }

  template <typename LegalOpFn>
  bool legalizeIntegerBinaryOp(llvm::BinaryOperator &BO,
                               LegalOpFn CreateLegalOp) {
    return legalizeIntegerBinaryOp(BO, CreateLegalOp, expandFail);
  }

  static bool expandWithRuntimeLib(llvm::IRBuilder<> &B,
                                   llvm::BinaryOperator &BO,
                                   llvm::StringRef FuncName);

  static bool expandAddWithCarry(llvm::IRBuilder<> &B,
                                 llvm::BinaryOperator &BO);
  static bool expandSubWithBorrow(llvm::IRBuilder<> &B,
                                  llvm::BinaryOperator &BO);
  static bool expandPackwise(llvm::IRBuilder<> &B, llvm::BinaryOperator &BO);

  static bool expandFail(llvm::IRBuilder<> &, llvm::BinaryOperator &BO) {
    WATEVER_LOG_WARN("Expanding {} not supported", BO.getOpcodeName());
    return false;
  };

public:
  explicit LegalizationPass(const llvm::Module &M) {
    Int8Ty = llvm::Type::getInt8Ty(M.getContext());
    Int16Ty = llvm::Type::getInt16Ty(M.getContext());
    Int32Ty = llvm::Type::getInt32Ty(M.getContext());
    Int64Ty = llvm::Type::getInt64Ty(M.getContext());

    // TODO support multiple address spaces
    PtrTy = llvm::PointerType::get(M.getContext(), 0);
    IntPtrTy = M.getDataLayout().getIntPtrType(M.getContext());
  }

  llvm::PreservedAnalyses run(llvm::Function &F,
                              llvm::FunctionAnalysisManager &AM);

  void visitAllocaInst(llvm::AllocaInst &) {};
  void visitBinaryOperator(llvm::BinaryOperator &BO);
  void visitGetElementPtrInst(llvm::GetElementPtrInst &GI);
  void visitLoadInst(llvm::LoadInst &LI);
  void visitRet(llvm::ReturnInst &RI);
  void visitSExtInst(llvm::SExtInst &SI);
  void visitStoreInst(llvm::StoreInst &SI);
  void visitTruncInst(llvm::TruncInst &TI);
  void visitUnaryOperator(llvm::UnaryOperator &UO);
  void visitZExtInst(llvm::ZExtInst &ZI);

  // Fail by default
  void visitInstruction(llvm::Instruction &I) {
    // TODO set to UNIMPLEMENTED
    WATEVER_TODO("{} not (yet) supported", I.getOpcodeName());
  }
};
} // namespace watever
