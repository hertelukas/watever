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
#include <llvm/Support/Casting.h>

#include "watever/utils.hpp"

namespace watever {

class FunctionLegalizer : public llvm::InstVisitor<FunctionLegalizer> {

  // Maps Old Value -> New Legal Value
  llvm::DenseMap<llvm::Value *, llvm::Value *> ValueMap{};
  llvm::IRBuilder<> &Builder;

  llvm::Value *legalizeConstant(llvm::Constant *C);

  llvm::Value *getMappedValue(llvm::Value *OldVal) {
    // Value is mapped
    if (auto It = ValueMap.find(OldVal); It != ValueMap.end()) {
      return It->second;
    }

    // Value is not mapped, it might be a constant
    if (auto *C = llvm::dyn_cast<llvm::Constant>(OldVal)) {
      return legalizeConstant(C);
    }

    WATEVER_UNREACHABLE("No value found for {}", llvmToString(*OldVal));
  }

  // Sign-extends val from `From` to `To` bits. Note, that `Val` might already
  // be `To` bits wide, but this ensures that the entire `To` bits are signed
  // correctly. This is needed if we cannot ignore the upper bits, and sign
  // matters. (sdiv)
  llvm::Value *signExtendTo(llvm::Value *Val, unsigned From, unsigned To) {

    // If the original value had the same width, we don't need to do anything
    if (To == From) {
      return Val;
    }

    // Supported natively
    if (To == 64 && From == 32 && Val->getType()->getIntegerBitWidth() == 32) {
      return Builder.CreateSExt(Val, llvm::Type::getInt64Ty(Val->getContext()));
    }

    llvm::Type *TargetTy = nullptr;
    if (To == 32) {
      TargetTy = llvm::Type::getInt32Ty(Val->getContext());
    } else if (To == 64) {
      TargetTy = llvm::Type::getInt64Ty(Val->getContext());
    } else {
      WATEVER_UNIMPLEMENTED("Unsupported target sign extension to {}", To);
    }

    // TODO check for --enable-sign-extension
    auto *WideOperand = Builder.CreateZExt(Val, TargetTy);
    auto *MaskVal = llvm::ConstantInt::get(TargetTy, To - From);
    llvm::Value *Shl = Builder.CreateShl(WideOperand, MaskVal);
    llvm::Value *Shr = Builder.CreateAShr(Shl, MaskVal);
    return Shr;
  }

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

  void visitBasicBlock(llvm::BasicBlock &BB);

  // Terminator Instructions
  void visitReturnInst(llvm::ReturnInst &RI);

  // Unary Operations

  // Binary Operations
  void visitBinaryOperator(llvm::BinaryOperator &BO);
  // Bitwise Binary Operations

  // Vector Operations

  // Aggregatge Operations

  // Memory Access and Addressing Operations

  // Conversion Operations

  // Other Operations

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
