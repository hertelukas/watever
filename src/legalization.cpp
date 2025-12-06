#include "watever/legalization.hpp"
#include "watever/utils.hpp"
#include <llvm/ADT/STLExtras.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/Support/Casting.h>

using namespace watever;

llvm::Value *FunctionLegalizer::legalizeConstant(llvm::Constant *C) {
  if (auto *CI = llvm::dyn_cast<llvm::ConstantInt>(C)) {
    const unsigned Width = CI->getBitWidth();
    if (Width == 32 || Width == 64) {
      return C;
    }
    if (Width < 32) {
      return llvm::ConstantInt::get(Builder.getInt32Ty(), CI->getZExtValue());
    }
    if (Width < 64) {
      return llvm::ConstantInt::get(Builder.getInt64Ty(), CI->getZExtValue());
    }
  }

  if (C->getType()->isFloatTy() || C->getType()->isDoubleTy()) {
    return C;
  }

  WATEVER_UNIMPLEMENTED("unsupported constant type {}", llvmToString(*C));
}

void FunctionLegalizer::visitBasicBlock(llvm::BasicBlock &BB) {
  if (auto *NewBB = llvm::dyn_cast<llvm::BasicBlock>(ValueMap[&BB])) {
    Builder.SetInsertPoint(NewBB);
    return;
  }
  WATEVER_UNREACHABLE("Corresponding BB not found in new function");
}

//===----------------------------------------------------------------------===//
// Terminator Instructions
//===----------------------------------------------------------------------===//
void FunctionLegalizer::visitReturnInst(llvm::ReturnInst &RI) {
  WATEVER_LOG_TRACE("legalizing ret {}", llvmToString(RI));
  Builder.CreateRet(ValueMap[RI.getReturnValue()]);
}

//===----------------------------------------------------------------------===//
// Unary Operations
//===----------------------------------------------------------------------===//
//===----------------------------------------------------------------------===//
// Binary Operations
//===----------------------------------------------------------------------===//
void FunctionLegalizer::visitBinaryOperator(llvm::BinaryOperator &BO) {
  WATEVER_LOG_TRACE("legalizing binop {}", llvmToString(BO));
  auto *LHS = getMappedValue(BO.getOperand(0));
  auto *RHS = getMappedValue(BO.getOperand(1));

  // TODO this is not generally legal, e.g., when dividing
  llvm::Value *NewBO = nullptr;
  switch (BO.getOpcode()) {
  case llvm::Instruction::Add:
  case llvm::Instruction::FAdd:
  case llvm::Instruction::Sub:
  case llvm::Instruction::FSub:
  case llvm::Instruction::Mul:
  case llvm::Instruction::FMul:
    break;
  case llvm::Instruction::UDiv: {
    LHS = zeroExtend(LHS, BO.getOperand(0)->getType()->getIntegerBitWidth(),
                     LHS->getType()->getIntegerBitWidth());
    RHS = zeroExtend(RHS, BO.getOperand(1)->getType()->getIntegerBitWidth(),
                     RHS->getType()->getIntegerBitWidth());
    break;
  }
  case llvm::Instruction::SDiv: {
    LHS = signExtend(LHS, BO.getOperand(0)->getType()->getIntegerBitWidth(),
                     LHS->getType()->getIntegerBitWidth());
    RHS = signExtend(RHS, BO.getOperand(1)->getType()->getIntegerBitWidth(),
                     RHS->getType()->getIntegerBitWidth());
    break;
  }
  case llvm::Instruction::FDiv:
    break;
  case llvm::Instruction::URem:
  case llvm::Instruction::SRem:
  case llvm::Instruction::FRem:
  case llvm::Instruction::Shl:
  case llvm::Instruction::LShr:
  case llvm::Instruction::AShr:
  case llvm::Instruction::And:
  case llvm::Instruction::Or:
  case llvm::Instruction::Xor:
    break;
  default:
    WATEVER_UNREACHABLE("Illegal opcode encountered: {}", BO.getOpcodeName());
  }
  NewBO = Builder.CreateBinOp(BO.getOpcode(), LHS, RHS);

  ValueMap[&BO] = NewBO;
}

//===----------------------------------------------------------------------===//
// Bitwise Binary Operations
//===----------------------------------------------------------------------===//
//===----------------------------------------------------------------------===//
// Vector Operations
//===----------------------------------------------------------------------===//
//===----------------------------------------------------------------------===//
// Aggregate Operations
//===----------------------------------------------------------------------===//
//===----------------------------------------------------------------------===//
// Memory Access and Addressing Operations
//===----------------------------------------------------------------------===//
//===----------------------------------------------------------------------===//
// Conversion Operations
//===----------------------------------------------------------------------===//
//===----------------------------------------------------------------------===//
// Other Operations
//===----------------------------------------------------------------------===//

llvm::Function *LegalizationPass::createLegalFunction(llvm::Module &Mod,
                                                      llvm::Function *OldFunc) {

  // TODO not correct, e.g., vectors are returned through pointer
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
  if (Ty->isVoidTy()) {
    return Ty;
  }
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
