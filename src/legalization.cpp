#include "watever/legalization.hpp"
#include "watever/utils.hpp"
#include <llvm/ADT/STLExtras.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/Support/Casting.h>

using namespace watever;

LegalValue FunctionLegalizer::legalizeConstant(llvm::Constant *C) {
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

    unsigned NumParts = (Width + 63) / 64;
    auto Value = CI->getValue();

    llvm::SmallVector<llvm::Value *> Vs;
    for (unsigned I = 0; I < NumParts; ++I) {
      // TODO think about order
      Vs.push_back(llvm::ConstantInt::get(
          Builder.getInt64Ty(), Value.extractBitsAsZExtValue(64, I * 64)));
    }

    return LegalValue{Vs};
  }

  if (C->getType()->isFloatTy() || C->getType()->isDoubleTy()) {
    return C;
  }

  WATEVER_UNIMPLEMENTED("unsupported constant type {}", llvmToString(*C));
}

FunctionLegalizer::FunctionLegalizer(llvm::Function *OldFunc,
                                     llvm::Function *NewFunc,
                                     llvm::IRBuilder<> &B,
                                     const TargetConfig &Config)
    : Builder(B), Config(Config), NewFunc(NewFunc) {

  bool IndirectReturn =
      LegalizationPass::getLegalType(OldFunc->getReturnType()).size() > 1;

  size_t I = IndirectReturn ? 1 : 0;
  for (auto &OldArg : OldFunc->args()) {
    auto LegalArgType = LegalizationPass::getLegalType(OldArg.getType());

    if (LegalArgType.size() == 1) {
      ValueMap[&OldArg] = LegalValue{NewFunc->getArg(I)};
    } else {
      llvm::SmallVector<llvm::Value *> MappedToValues{};
      for (size_t J = 0; J < LegalArgType.size(); ++J) {
        MappedToValues.push_back(NewFunc->getArg(I + J));
      }

      ValueMap[&OldArg] = LegalValue{MappedToValues};
    }

    I += LegalArgType.size();
  }

  for (auto &OldBB : *OldFunc) {
    llvm::BasicBlock *NewBB = llvm::BasicBlock::Create(
        NewFunc->getContext(), OldBB.getName(), NewFunc);
    ValueMap[&OldBB] = NewBB;
  }

  Int8Ty = llvm::Type::getInt8Ty(OldFunc->getContext());
  Int16Ty = llvm::Type::getInt16Ty(OldFunc->getContext());
  Int32Ty = llvm::Type::getInt32Ty(OldFunc->getContext());
  Int64Ty = llvm::Type::getInt64Ty(OldFunc->getContext());

  // TODO support multiple address spaces
  PtrTy = llvm::PointerType::get(OldFunc->getContext(), 0);
  IntPtrTy = OldFunc->getDataLayout().getIntPtrType(OldFunc->getContext());
}

void FunctionLegalizer::visitBasicBlock(llvm::BasicBlock &BB) {
  if (auto *NewBB = llvm::dyn_cast<llvm::BasicBlock>(ValueMap[&BB][0])) {
    Builder.SetInsertPoint(NewBB);
    return;
  }
  WATEVER_UNREACHABLE("Corresponding BB not found in new function");
}

//===----------------------------------------------------------------------===//
// Terminator Instructions
//===----------------------------------------------------------------------===//
void FunctionLegalizer::visitReturnInst(llvm::ReturnInst &RI) {
  WATEVER_LOG_TRACE("legalizing {}", llvmToString(RI));

  if (RI.getNumOperands() == 0) {
    Builder.CreateRetVoid();
    return;
  }

  auto LegalReturnValue = getMappedValue(RI.getReturnValue());

  if (!LegalReturnValue.isScalar()) {
    WATEVER_LOG_TRACE("storing return value in first argument");
    auto *ReturnPtr = NewFunc->getArg(0);
    auto *ReturnPtrAsInt = Builder.CreatePtrToInt(ReturnPtr, IntPtrTy);
    unsigned I = 0;
    for (auto *ReturnValue : LegalReturnValue) {
      auto *CurrentPtrAsInt = Builder.CreateAdd(
          ReturnPtrAsInt, llvm::ConstantInt::get(IntPtrTy, I));
      auto *CurrentPtr = Builder.CreateIntToPtr(CurrentPtrAsInt, PtrTy);
      Builder.CreateAlignedStore(ReturnValue, CurrentPtr, llvm::Align(8));
      I += 8;
    }
    Builder.CreateRetVoid();
    return;
  }

  if (LegalReturnValue.isScalar()) {
    Builder.CreateRet(LegalReturnValue[0]);
    return;
  }
}

//===----------------------------------------------------------------------===//
// Unary Operations
//===----------------------------------------------------------------------===//
void FunctionLegalizer::visitUnaryOperator(llvm::UnaryOperator &UO) {
  // TODO support vectors
  auto *LegalOperand = getMappedValue(UO.getOperand(0))[0];
  switch (UO.getOpcode()) {
  case llvm::Instruction::FNeg:
    if (UO.getType()->isDoubleTy() || UO.getType()->isFloatTy()) {
      ValueMap[&UO] = Builder.CreateFNeg(LegalOperand);
      return;
    }
    WATEVER_TODO("handle fneg of type", llvmToString(*UO.getType()));
    break;
  default:
    WATEVER_UNREACHABLE("Illegal unary opcode encountered: {}",
                        UO.getOpcodeName());
    break;
  }
}

//===----------------------------------------------------------------------===//
// Binary Operations
//===----------------------------------------------------------------------===//
void FunctionLegalizer::visitBinaryOperator(llvm::BinaryOperator &BO) {
  WATEVER_LOG_TRACE("legalizing binop {}", llvmToString(BO));
  LegalValue LegalLHS = getMappedValue(BO.getOperand(0));
  LegalValue LegalRHS = getMappedValue(BO.getOperand(1));

#ifdef WATEVER_LOGGING
  if (LegalLHS.size() != LegalRHS.size()) {
    WATEVER_UNREACHABLE(
        "binary operators must have equally sized LHS and RHS!");
  }
#endif

  if (LegalLHS.isScalar()) {
    auto *LHS = LegalLHS[0];
    auto *RHS = LegalRHS[0];
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
    case llvm::Instruction::URem: {
      LHS = zeroExtend(LHS, BO.getOperand(0)->getType()->getIntegerBitWidth(),
                       LHS->getType()->getIntegerBitWidth());
      RHS = zeroExtend(RHS, BO.getOperand(1)->getType()->getIntegerBitWidth(),
                       RHS->getType()->getIntegerBitWidth());
      break;
    }
    case llvm::Instruction::SRem: {
      LHS = signExtend(LHS, BO.getOperand(0)->getType()->getIntegerBitWidth(),
                       LHS->getType()->getIntegerBitWidth());
      RHS = signExtend(RHS, BO.getOperand(1)->getType()->getIntegerBitWidth(),
                       RHS->getType()->getIntegerBitWidth());
      break;
    }
    case llvm::Instruction::FRem: {
      WATEVER_UNIMPLEMENTED("Support frem");
    }
    case llvm::Instruction::Shl: {
      // we don't care about upper bits in LHS, as they are shifted away anyway
      RHS = zeroExtend(RHS, BO.getOperand(1)->getType()->getIntegerBitWidth(),
                       RHS->getType()->getIntegerBitWidth());
      break;
    }
    case llvm::Instruction::LShr: {
      LHS = zeroExtend(LHS, BO.getOperand(0)->getType()->getIntegerBitWidth(),
                       LHS->getType()->getIntegerBitWidth());
      RHS = zeroExtend(RHS, BO.getOperand(1)->getType()->getIntegerBitWidth(),
                       RHS->getType()->getIntegerBitWidth());
      break;
    }
    case llvm::Instruction::AShr: {
      LHS = signExtend(LHS, BO.getOperand(0)->getType()->getIntegerBitWidth(),
                       LHS->getType()->getIntegerBitWidth());
      RHS = zeroExtend(RHS, BO.getOperand(1)->getType()->getIntegerBitWidth(),
                       RHS->getType()->getIntegerBitWidth());
      break;
    }
    case llvm::Instruction::And:
    case llvm::Instruction::Or:
    case llvm::Instruction::Xor:
      break;
    default:
      WATEVER_UNREACHABLE("Illegal opcode encountered: {}", BO.getOpcodeName());
    }

    ValueMap[&BO] = Builder.CreateBinOp(BO.getOpcode(), LHS, RHS);

    return;
  }

  // TODO vectors
  switch (BO.getOpcode()) {

  case llvm::Instruction::Add: {
    // TODO there might be a more efficient solution, than checking the carry
    // twice
    llvm::Value *Carry = llvm::ConstantInt::get(Int64Ty, 0);
    llvm::SmallVector<llvm::Value *> Result;
    for (auto [LHS, RHS] : llvm::zip_equal(LegalLHS, LegalRHS)) {
      llvm::Value *Sum1 = Builder.CreateAdd(LHS, RHS);
      llvm::Value *Carry1 = Builder.CreateICmpULT(Sum1, LHS);
      llvm::Value *SumFinal = Builder.CreateAdd(Sum1, Carry);
      llvm::Value *Carry2 = Builder.CreateICmpULT(SumFinal, Sum1);
      // TODO could be skipped in last round
      llvm::Value *CarryBit = Builder.CreateOr(Carry1, Carry2);
      Carry = Builder.CreateZExt(CarryBit, Int64Ty);
      Result.push_back(SumFinal);
    }
    ValueMap[&BO] = LegalValue{Result};
    return;
  }
  case llvm::Instruction::FAdd:
  case llvm::Instruction::Sub:
  case llvm::Instruction::FSub:
  case llvm::Instruction::Mul:
  case llvm::Instruction::FMul:
  case llvm::Instruction::UDiv:
  case llvm::Instruction::SDiv:
  case llvm::Instruction::FDiv:
  case llvm::Instruction::URem:
  case llvm::Instruction::SRem:
  case llvm::Instruction::FRem:
  case llvm::Instruction::Shl:
  case llvm::Instruction::LShr:
  case llvm::Instruction::AShr:
  case llvm::Instruction::And:
  case llvm::Instruction::Or:
  case llvm::Instruction::Xor:
    WATEVER_UNIMPLEMENTED("unsupported long binop");
  default:
    WATEVER_UNREACHABLE("Illegal opcode encountered: {}", BO.getOpcodeName());
  }
}

//===----------------------------------------------------------------------===//
// Vector Operations
//===----------------------------------------------------------------------===//
//===----------------------------------------------------------------------===//
// Aggregate Operations
//===----------------------------------------------------------------------===//
//===----------------------------------------------------------------------===//
// Memory Access and Addressing Operations
//===----------------------------------------------------------------------===//

void FunctionLegalizer::visitGetElementPtrInst(llvm::GetElementPtrInst &GI) {
  const llvm::DataLayout &DL = GI.getModule()->getDataLayout();

  // TODO handle vectors
  auto *PtrArg = getMappedValue(GI.getPointerOperand())[0];

  llvm::APInt Offset(DL.getPointerSizeInBits(GI.getPointerAddressSpace()), 0);
  if (GI.accumulateConstantOffset(DL, Offset)) {
    WATEVER_LOG_TRACE("GEP with constant offset of {}", Offset.getSExtValue());
    if (Offset.isZero()) {
      // Just send the ptr itself
      ValueMap[&GI] = PtrArg;
    } else {
      llvm::Value *OffsetVal = llvm::ConstantInt::get(IntPtrTy, Offset);
      llvm::Value *PtrAsInt = Builder.CreatePtrToInt(PtrArg, IntPtrTy);
      llvm::Value *PtrWithOffsetAsInt = Builder.CreateAdd(PtrAsInt, OffsetVal);
      llvm::Value *PtrWithOffset =
          Builder.CreateIntToPtr(PtrWithOffsetAsInt, PtrTy);
      ValueMap[&GI] = PtrWithOffset;
    }
    return;
  }

  llvm::Type *CurrentTy = GI.getSourceElementType();
  llvm::Value *TotalOffset = llvm::ConstantInt::get(IntPtrTy, 0);

  const auto *IdxIt = GI.idx_begin();

  llvm::Value *FirstIndex = getMappedValue(IdxIt->get())[0];
  uint64_t FirstTypeSize = DL.getTypeAllocSize(CurrentTy);
  llvm::Value *SextIdx = Builder.CreateSExtOrTrunc(FirstIndex, IntPtrTy);

  WATEVER_LOG_TRACE("Accessing type at index {}, with a type size of {}",
                    llvmToString(*SextIdx), FirstTypeSize);

  if (FirstTypeSize != 0) {
    TotalOffset = Builder.CreateMul(
        SextIdx, llvm::ConstantInt::get(IntPtrTy, FirstTypeSize));
  }

  ++IdxIt;

  for (; IdxIt != GI.idx_end(); ++IdxIt) {
    llvm::Value *Index = getMappedValue(IdxIt->get())[0];
    SextIdx = Builder.CreateSExtOrTrunc(Index, IntPtrTy);

    if (llvm::StructType *STy = llvm::dyn_cast<llvm::StructType>(CurrentTy)) {
      // TODO can we index into a struct with a variable?
      unsigned FieldIdx = cast<llvm::ConstantInt>(Index)->getZExtValue();
      uint64_t FieldOffset =
          DL.getStructLayout(STy)->getElementOffset(FieldIdx);

      WATEVER_LOG_TRACE("Accessing struct at index {}, with offset of {}",
                        FieldIdx, FieldOffset);

      TotalOffset = Builder.CreateAdd(
          TotalOffset, llvm::ConstantInt::get(IntPtrTy, FieldOffset));
      CurrentTy = STy->getElementType(FieldIdx);
    } else {
      CurrentTy = CurrentTy->getContainedType(0);
      uint64_t ElementSize = DL.getTypeAllocSize(CurrentTy);

      WATEVER_LOG_TRACE("Accessing array at index {}, with element sizes of {}",
                        llvmToString(*SextIdx), ElementSize);

      llvm::Value *ScaledOffset = Builder.CreateMul(
          SextIdx, llvm::ConstantInt::get(IntPtrTy, ElementSize));
      TotalOffset = Builder.CreateAdd(TotalOffset, ScaledOffset);
    }
  }

  llvm::Value *BasePtrInt = Builder.CreatePtrToInt(PtrArg, IntPtrTy);
  llvm::Value *NewPtrInt = Builder.CreateAdd(BasePtrInt, TotalOffset);
  llvm::Value *NewPtr = Builder.CreateIntToPtr(NewPtrInt, PtrTy);

  ValueMap[&GI] = NewPtr;
}

//===----------------------------------------------------------------------===//
// Conversion Operations
//===----------------------------------------------------------------------===//
//===----------------------------------------------------------------------===//
// Other Operations
//===----------------------------------------------------------------------===//

llvm::Function *LegalizationPass::createLegalFunction(llvm::Module &Mod,
                                                      llvm::Function *OldFunc) {

  llvm::Function *Fn = llvm::Function::Create(
      createLegalFunctionType(OldFunc->getFunctionType()),
      OldFunc->getLinkage(), OldFunc->getName(), Mod);

  bool IndirectReturn = getLegalType(OldFunc->getReturnType()).size() > 1;

  size_t I = 0;
  if (IndirectReturn) {
    Fn->getArg(0)->setName("ret.ptr");
    ++I;
  }
  for (auto &OldArg : OldFunc->args()) {
    auto LegalArgType = LegalizationPass::getLegalType(OldArg.getType());

    if (LegalArgType.size() == 1) {
      Fn->getArg(I)->setName(OldArg.getName());
    } else {
      for (size_t J = 0; J < LegalArgType.size(); ++J) {
        Fn->getArg(I + J)->setName(OldArg.getName() + "." + llvm::Twine(J));
      }
    }

    I += LegalArgType.size();
  }

  return Fn;
}

llvm::FunctionType *
LegalizationPass::createLegalFunctionType(llvm::FunctionType *OldFuncTy) {
  auto LegalResultTy = getLegalType(OldFuncTy->getReturnType());

  llvm::Type *ResultTy = nullptr;
  llvm::SmallVector<llvm::Type *> Params;
  if (LegalResultTy.size() > 1) {
    // TODO the spec uses multi value return types, however, LLVM still compiles
    // with indirect types
    WATEVER_LOG_DBG("Return type not supported, using indirect return");
    ResultTy = llvm::Type::getVoidTy(OldFuncTy->getContext());
    Params.push_back(llvm::PointerType::getUnqual(OldFuncTy->getContext()));
  } else {
    ResultTy = LegalResultTy[0];
  }

  for (auto *OldParamType : OldFuncTy->params()) {
    auto LegalParamType = getLegalType(OldParamType);
    for (auto &Ty : LegalParamType) {
      Params.push_back(Ty);
    }
  }

  return llvm::FunctionType::get(ResultTy, Params, OldFuncTy->isVarArg());
}

LegalType LegalizationPass::getLegalType(llvm::Type *Ty) {
  if (Ty->isVoidTy()) {
    return Ty;
  }
  if (Ty->isIntegerTy()) {
    unsigned Width = Ty->getIntegerBitWidth();
    if (Width <= 32) {
      return llvm::Type::getInt32Ty(Ty->getContext());
    }
    if (Width <= 64) {
      return llvm::Type::getInt64Ty(Ty->getContext());
    }

    unsigned NumParts = (Width + 63) / 64;
    llvm::SmallVector<llvm::Type *> Ts(
        NumParts, llvm::Type::getInt64Ty(Ty->getContext()));
    return LegalType{Ts};
  }

  if (Ty->isFloatingPointTy()) {
    if (Ty->isFloatTy() || Ty->isDoubleTy()) {
      return Ty;
    }
  }

  if (Ty->isPointerTy()) {
    return Ty;
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
    FunctionLegalizer FL{F, NewFunc, Builder, Config};
    FL.visit(F);
    FL.NewFunc->takeName(F);
    WATEVER_LOG_DBG("Legalized Function:\n {}", llvmToString(*FL.NewFunc));
    F->eraseFromParent();
  }

  return llvm::PreservedAnalyses::none();
}
