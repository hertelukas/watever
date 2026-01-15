#include "watever/legalization.hpp"
#include "watever/utils.hpp"
#include <cstdint>
#include <llvm/ADT/APInt.h>
#include <llvm/ADT/DenseMapInfo.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/Attributes.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DataLayout.h>
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

  if (C->getType()->isPointerTy()) {
    return C;
  }

  if (llvm::isa<llvm::PoisonValue>(C) || llvm::isa<llvm::UndefValue>(C)) {
    return C;
  }

  WATEVER_UNIMPLEMENTED("unsupported constant type {}", llvmToString(*C));
}

void FunctionLegalizer::fixupPHIs() {
  for (auto *OldPN : PHIsToFix) {
    LegalValue NewPNs = ValueMap[OldPN];

    for (auto [value, block] :
         llvm::zip_equal(OldPN->incoming_values(), OldPN->blocks())) {
      LegalValue NewIncomingVal = getMappedValue(value);
      auto *NewIncomingBB = llvm::cast<llvm::BasicBlock>(ValueMap[block][0]);
      for (auto [NewPHIValPart, NewIncomingValPart] :
           llvm::zip_equal(NewPNs, NewIncomingVal)) {
        auto *NewPHI = llvm::cast<llvm::PHINode>(NewPHIValPart);
        NewPHI->addIncoming(NewIncomingValPart, NewIncomingBB);
      }
    }
  }
}

FunctionLegalizer::FunctionLegalizer(
    llvm::Function *OldFunc, llvm::Function *NewFunc, llvm::IRBuilder<> &B,
    const TargetConfig &Config,
    const llvm::DenseMap<llvm::Function *, llvm::Function *> &FuncMap)
    : Builder(B), Config(Config), FuncMap(FuncMap), NewFunc(NewFunc) {

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

  Int1Ty = llvm::Type::getInt1Ty(OldFunc->getContext());
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
    // This might happen if we return the result of a comparison
    if (LegalReturnValue[0]->getType()->isIntegerTy(1)) {
      auto *ReturnValue = Builder.CreateZExt(LegalReturnValue[0], Int32Ty);
      Builder.CreateRet(ReturnValue);
      return;
    }
    Builder.CreateRet(LegalReturnValue[0]);
    return;
  }
}

void FunctionLegalizer::visitBranchInst(llvm::BranchInst &BI) {
  auto GetMappedBlock = [&](unsigned SuccIdx) -> llvm::BasicBlock * {
    auto Mapped = getMappedValue(BI.getSuccessor(SuccIdx));
    return llvm::dyn_cast<llvm::BasicBlock>(Mapped[0]);
  };

  if (BI.isConditional()) {
    auto *True = GetMappedBlock(0);
    auto *False = GetMappedBlock(1);
    auto *Cond = getMappedValue(BI.getCondition())[0];

    Cond = Builder.CreateAnd(Cond, 1);
    Cond = Builder.CreateTrunc(Cond, Int1Ty);

    ValueMap[&BI] = Builder.CreateCondBr(Cond, True, False);
    return;
  }

  auto *NewSuccessor = GetMappedBlock(0);
  ValueMap[&BI] = Builder.CreateBr(NewSuccessor);
}

void FunctionLegalizer::visitSwitchInst(llvm::SwitchInst &SI) {
  auto Cond = getMappedValue(SI.getCondition());
  if (!Cond.isScalar()) {
    WATEVER_UNIMPLEMENTED("switch on non-scalar value");
  }

  // TODO Handle non i32 conditions
  assert(Cond[0]->getType()->isIntegerTy(32) && "condition must be 32-bit");
  auto *CondVal = Cond[0];
  if (SI.getCondition()->getType() != CondVal->getType()) {
    CondVal = Builder.CreateAnd(
        CondVal, llvm::APInt::getLowBitsSet(
                     32, SI.getCondition()->getType()->getIntegerBitWidth()));
  }
  auto *DefaultDest =
      llvm::dyn_cast<llvm::BasicBlock>(getMappedValue(SI.getDefaultDest())[0]);

  auto *NewSI = Builder.CreateSwitch(CondVal, DefaultDest, SI.getNumCases());
  for (auto Case : SI.cases()) {
    LegalValue CaseConstant = legalizeConstant(Case.getCaseValue());
    assert(CaseConstant.isScalar() && "case constant must be scalar");

    auto *NewCase = llvm::dyn_cast<llvm::ConstantInt>(CaseConstant[0]);
    auto *NewSucc = llvm::dyn_cast<llvm::BasicBlock>(
        getMappedValue(Case.getCaseSuccessor())[0]);
    NewSI->addCase(NewCase, NewSucc);
  }
  ValueMap[&SI] = LegalValue{NewSI};
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
      // we don't care about upper bits in LHS, as they are shifted away
      // anyway
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

    ValueMap[&BO] = Builder.CreateBinOp(BO.getOpcode(), LHS, RHS, BO.getName());

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
      Carry1 = Builder.CreateZExt(Carry1, Int64Ty);
      Carry2 = Builder.CreateZExt(Carry2, Int64Ty);
      Carry = Builder.CreateOr(Carry1, Carry2);
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

void FunctionLegalizer::visitAllocaInst(llvm::AllocaInst &AI) {
  llvm::Value *ArraySize = AI.getArraySize();

  if (!llvm::isa<llvm::Constant>(ArraySize)) {
    ArraySize = getMappedValue(ArraySize)[0];
  }
  auto *NewAI = Builder.CreateAlloca(
      AI.getAllocatedType(), AI.getAddressSpace(), ArraySize, AI.getName());
  NewAI->setAlignment(AI.getAlign());
  ValueMap[&AI] = NewAI;
}

void FunctionLegalizer::visitLoadInst(llvm::LoadInst &LI) {
  auto *ResultType = LI.getType();
  auto *Pointer = getMappedValue(LI.getPointerOperand())[0];

  if (ResultType->isIntegerTy()) {
    const unsigned Width = ResultType->getIntegerBitWidth();

    // We can just load the int
    if (Width == 32 || Width == 64) {
      ValueMap[&LI] = Builder.CreateAlignedLoad(ResultType, Pointer,
                                                LI.getAlign(), LI.getName());
      return;
    }
    if (Width > 64) {
      WATEVER_UNIMPLEMENTED("expanding load not supported");
      return;
    }

    // In these cases, it's not possible to create multiple loads to
    llvm::Type *TypeToLoad = nullptr;
    llvm::Type *TargetType = nullptr;
    if (Width <= 8) {
      TypeToLoad = Int8Ty;
      TargetType = Int32Ty;
    } else if (Width <= 16) {
      TypeToLoad = Int16Ty;
      TargetType = Int32Ty;
    } else if (16 + 8 < Width && Width < 32) {
      TypeToLoad = Int32Ty;
      TargetType = Int32Ty;
    } else if (32 + 16 + 8 < Width) {
      TypeToLoad = Int64Ty;
      TargetType = Int64Ty;
    }

    if (TypeToLoad) {
      llvm::Value *Result = Builder.CreateAlignedLoad(
          TypeToLoad, Pointer, LI.getAlign(), LI.getName());

      if (TypeToLoad != TargetType) {
        Result = Builder.CreateZExt(Result, TargetType);
      }
      ValueMap[&LI] = Result;
      return;
    }

    // Use multiple loads as near around the requested width (E.g., a load i56
    // is a load 32, load 16, load 8)
    llvm::Value *Result;
    // We want to build an i64 with the result
    if (Width > 32) {
      Result = Builder.CreateLoad(Int32Ty, Pointer);
      Result = Builder.CreateZExt(Result, Int64Ty);
      auto *PtrAsInt = Builder.CreatePtrToInt(Pointer, IntPtrTy);
      unsigned BytesLoaded = 4;
      // For (40, 56]
      if (Width > 40) {
        llvm::Value *NewPtrAsInt = Builder.CreateAdd(
            PtrAsInt, llvm::ConstantInt::get(IntPtrTy, BytesLoaded));
        llvm::Value *NewPtr = Builder.CreateIntToPtr(NewPtrAsInt, PtrTy);
        llvm::Value *NextTwoBytes = Builder.CreateLoad(Int16Ty, NewPtr);
        NextTwoBytes = Builder.CreateZExt(NextTwoBytes, Int64Ty);
        llvm::Value *ShiftAmount =
            llvm::ConstantInt::get(Int64Ty, BytesLoaded * 8);
        NextTwoBytes = Builder.CreateShl(NextTwoBytes, ShiftAmount);
        Result = Builder.CreateOr(Result, NextTwoBytes);
        NewPtrAsInt =
            Builder.CreateAdd(NewPtrAsInt, llvm::ConstantInt::get(IntPtrTy, 2));
        BytesLoaded += 2;
      }
      // For (32, 40], and (48, 56]
      if (Width <= 40 || Width > 48) {
        llvm::Value *NewPtrAsInt = Builder.CreateAdd(
            PtrAsInt, llvm::ConstantInt::get(IntPtrTy, BytesLoaded));
        llvm::Value *NewPtr = Builder.CreateIntToPtr(NewPtrAsInt, PtrTy);
        llvm::Value *NextByte = Builder.CreateLoad(Int8Ty, NewPtr);
        NextByte = Builder.CreateZExt(NextByte, Int64Ty);
        llvm::Value *ShiftAmount =
            llvm::ConstantInt::get(Int64Ty, BytesLoaded * 8);
        NextByte = Builder.CreateShl(NextByte, ShiftAmount);
        Result = Builder.CreateOr(Result, NextByte);
      }
    } else {
      // Width is between 17 and 24
      // Load the lowest 16 bit
      Result = Builder.CreateLoad(Int16Ty, Pointer);
      Result = Builder.CreateZExt(Result, Int32Ty);

      // Load the next 8 bit; add two to the pointer
      llvm::Value *NextOffset = llvm::ConstantInt::get(IntPtrTy, 2);
      llvm::Value *PtrAsInt = Builder.CreatePtrToInt(Pointer, IntPtrTy);
      llvm::Value *NewPtrAsInt = Builder.CreateAdd(PtrAsInt, NextOffset);
      Pointer = Builder.CreateIntToPtr(NewPtrAsInt, PtrTy);

      llvm::Value *NextByte = Builder.CreateLoad(Int8Ty, Pointer);
      NextByte = Builder.CreateZExt(NextByte, Int32Ty);
      NextByte = Builder.CreateShl(NextByte, 16);
      Result = Builder.CreateOr(Result, NextByte);
    }
    ValueMap[&LI] = Result;
    return;
  }
  if (ResultType->isFloatingPointTy()) {
    if (ResultType->isDoubleTy() || ResultType->isFloatTy()) {
      ValueMap[&LI] = Builder.CreateAlignedLoad(ResultType, Pointer,
                                                LI.getAlign(), LI.getName());
      return;
    }
    WATEVER_UNIMPLEMENTED("handle load of unsupported floating point type",
                          llvmToString(*ResultType));
    return;
  }
  if (ResultType->isPointerTy()) {
    ValueMap[&LI] =
        Builder.CreateAlignedLoad(PtrTy, Pointer, LI.getAlign(), LI.getName());
    return;
  }
}

void FunctionLegalizer::visitStoreInst(llvm::StoreInst &SI) {
  auto *StoreType = SI.getValueOperand()->getType();
  auto StoreValue = getMappedValue(SI.getValueOperand());
  auto *Pointer = getMappedValue(SI.getPointerOperand())[0];

  if (StoreType->isIntegerTy()) {
    const unsigned Width = StoreType->getIntegerBitWidth();
    if (Width == 32 || Width == 64) {
      ValueMap[&SI] =
          Builder.CreateAlignedStore(StoreValue[0], Pointer, SI.getAlign());
      return;
    }

    if (Width > 64) {
      WATEVER_UNIMPLEMENTED("expanding store not supported");
      return;
    }

    llvm::Value *ToStore = nullptr;

    // We have to make sure that we don't store unclean bits
    // TODO not sure if we do need to check this
    if (Width % 8 != 0) {
      ToStore = Builder.CreateAnd(
          StoreValue[0],
          llvm::APInt::getLowBitsSet(
              StoreValue[0]->getType()->getIntegerBitWidth(), Width));
    } else {
      ToStore = StoreValue[0];
    }

    // Can't split
    if (Width > 32 + 16 + 8 || (Width < 32 && Width > 16 + 8)) {
      ValueMap[&SI] =
          Builder.CreateAlignedStore(ToStore, Pointer, SI.getAlign());
      return;
    }

    llvm::Value *Res = nullptr;

    if (Width > 32) {
      llvm::Value *LowestBytes = Builder.CreateTrunc(ToStore, Int32Ty);
      Builder.CreateStore(LowestBytes, Pointer);

      llvm::Value *PtrAsInt = Builder.CreatePtrToInt(Pointer, IntPtrTy);

      unsigned BytesStored = 4;
      // The number is wide enough to require a 16-bit store
      if (Width > 32 + 8) {
        llvm::Value *NewPtrAsInt = Builder.CreateAdd(
            PtrAsInt, llvm::ConstantInt::get(IntPtrTy, BytesStored));
        llvm::Value *NewPtr = Builder.CreateIntToPtr(NewPtrAsInt, PtrTy);
        llvm::Value *NextBytes = Builder.CreateLShr(ToStore, BytesStored * 8);
        NextBytes = Builder.CreateTrunc(NextBytes, Int16Ty);
        Res = Builder.CreateStore(NextBytes, NewPtr);
        BytesStored += 2;
      }
      // If it wasn't wide enough (e.g., [33, 40]), we only need a 8-bit store
      // Or if it is still not fully stored (e.g., [49, 56])
      if (Width <= 32 + 8 || Width > 32 + 16) {
        llvm::Value *NewPtrAsInt = Builder.CreateAdd(
            PtrAsInt, llvm::ConstantInt::get(IntPtrTy, BytesStored));
        llvm::Value *NewPtr = Builder.CreateIntToPtr(NewPtrAsInt, PtrTy);
        llvm::Value *NextByte = Builder.CreateLShr(ToStore, BytesStored * 8);
        NextByte = Builder.CreateTrunc(NextByte, Int8Ty);
        Res = Builder.CreateStore(NextByte, NewPtr);
      }
    } else {
      // We need to store at least a 16-bit
      if (Width > 8) {
        llvm::Value *LowestBytes = Builder.CreateTrunc(ToStore, Int16Ty);
        Res = Builder.CreateStore(LowestBytes, Pointer);
      }
      if (Width <= 8 || Width > 16) {
        llvm::Value *Ptr = Pointer;
        // We are storing higher bytes
        if (Width > 16) {
          ToStore = Builder.CreateLShr(ToStore, 16);
          llvm::Value *IntPtr = Builder.CreatePtrToInt(Ptr, IntPtrTy);
          IntPtr =
              Builder.CreateAdd(IntPtr, llvm::ConstantInt::get(IntPtrTy, 2));
          Ptr = Builder.CreateIntToPtr(IntPtr, PtrTy);
        }
        llvm::Value *LowestByte = Builder.CreateTrunc(ToStore, Int8Ty);
        Res = Builder.CreateStore(LowestByte, Ptr);
      }
    }
    ValueMap[&SI] = Res;
    return;
  }
  if (StoreType->isFloatingPointTy()) {
    if (StoreType->isDoubleTy() || StoreType->isFloatTy()) {
      ValueMap[&SI] =
          Builder.CreateAlignedStore(StoreValue[0], Pointer, SI.getAlign());
      return;
    }
    WATEVER_UNIMPLEMENTED("handle store of unsupported floating point type",
                          llvmToString(*StoreType));
  }

  if (StoreType->isPointerTy()) {
    ValueMap[&SI] =
        Builder.CreateAlignedStore(StoreValue[0], Pointer, SI.getAlign());
    return;
  }
}

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

    if (auto *STy = llvm::dyn_cast<llvm::StructType>(CurrentTy)) {
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
void FunctionLegalizer::visitTruncInst(llvm::TruncInst &TI) {
  auto Arg = getMappedValue(TI.getOperand(0));

  const auto FromWidth = TI.getSrcTy()->getIntegerBitWidth();
  const auto ToWidth = TI.getDestTy()->getIntegerBitWidth();

  if (Arg.isScalar()) {
    // Emit a wrap
    if (FromWidth > 32 && ToWidth <= 32) {
      ValueMap[&TI] = Builder.CreateTrunc(Arg[0], Int32Ty, TI.getName());
    }
    // Otherwise, this is a no-op
    else {
      ValueMap[&TI] = Arg;
    }
    return;
  }

  WATEVER_UNIMPLEMENTED("non-scalar truncation from {} to {}",
                        llvmToString(*TI.getSrcTy()),
                        llvmToString(*TI.getDestTy()));
}

void FunctionLegalizer::visitZExtInst(llvm::ZExtInst &ZI) {
  auto Arg = getMappedValue(ZI.getOperand(0));
  const auto FromWidth = ZI.getSrcTy()->getIntegerBitWidth();
  const auto ToWidth = ZI.getDestTy()->getIntegerBitWidth();
  if (Arg.isScalar()) {
    auto *Val = Arg[0];
    // If we currently filled only parts of a Wasm value, we have to ensure that
    // the upper bits in the new value are clean
    if (FromWidth != 32) {
      Val = Builder.CreateAnd(
          Val, llvm::APInt::getLowBitsSet(Val->getType()->getIntegerBitWidth(),
                                          FromWidth));
    }
    if (ToWidth > 32 && FromWidth <= 32) {
      ValueMap[&ZI] = Builder.CreateZExt(Val, ZI.getDestTy());
    } else {
      ValueMap[&ZI] = Val;
    }
    return;
  }
  WATEVER_UNIMPLEMENTED("non-scalar zext from {} to {}",
                        llvmToString(*ZI.getSrcTy()),
                        llvmToString(*ZI.getDestTy()));
}

void FunctionLegalizer::visitSExtInst(llvm::SExtInst &SI) {
  auto Arg = getMappedValue(SI.getOperand(0));

  const auto FromWidth = SI.getSrcTy()->getIntegerBitWidth();
  const auto ToWidth = SI.getDestTy()->getIntegerBitWidth();

  if (Arg.isScalar()) {
    ValueMap[&SI] = signExtend(Arg[0], FromWidth, ToWidth);
    return;
  }
  WATEVER_UNIMPLEMENTED("non-scalar zext from {} to {}",
                        llvmToString(*SI.getSrcTy()),
                        llvmToString(*SI.getDestTy()));
}

void FunctionLegalizer::visitFPTruncInst(llvm::FPTruncInst &FI) {
  auto Arg = getMappedValue(FI.getOperand(0));

  if (Arg.isScalar()) {
    if (FI.getSrcTy()->isDoubleTy() && FI.getDestTy()->isFloatTy()) {
      ValueMap[&FI] = Builder.CreateFPTrunc(Arg[0], FI.getDestTy());
      return;
    }
  }

  WATEVER_UNIMPLEMENTED("fp trunc from {} to {}", llvmToString(*FI.getSrcTy()),
                        llvmToString(*FI.getDestTy()));
}

void FunctionLegalizer::visitFPExtInst(llvm::FPExtInst &FI) {
  auto Arg = getMappedValue(FI.getOperand(0));

  if (Arg.isScalar()) {
    if (FI.getSrcTy()->isFloatTy() && FI.getDestTy()->isDoubleTy()) {
      ValueMap[&FI] = Builder.CreateFPExt(Arg[0], FI.getDestTy());
      return;
    }
  }

  WATEVER_UNIMPLEMENTED("fp extension from {} to {}",
                        llvmToString(*FI.getSrcTy()),
                        llvmToString(*FI.getDestTy()));
}

void FunctionLegalizer::visitSIToFPInst(llvm::SIToFPInst &SI) {
  auto Arg = getMappedValue(SI.getOperand(0));
  auto *TargetTy = SI.getDestTy();

  if (!TargetTy->isDoubleTy() && !TargetTy->isFloatingPointTy()) {
    WATEVER_UNIMPLEMENTED("unsupported SI to FP type {}",
                          llvmToString(*TargetTy));
  }

  if (Arg.isScalar()) {
    auto *Extended = signExtend(Arg[0], SI.getSrcTy()->getIntegerBitWidth(),
                                Arg[0]->getType()->getIntegerBitWidth());
    ValueMap[&SI] = Builder.CreateSIToFP(Extended, TargetTy);
    return;
  }
  WATEVER_UNIMPLEMENTED("non-scalar SI {} to FP {}",
                        llvmToString(*SI.getSrcTy()),
                        llvmToString(*SI.getDestTy()));
}
//===----------------------------------------------------------------------===//
// Other Operations
//===----------------------------------------------------------------------===//

void FunctionLegalizer::visitICmpInst(llvm::ICmpInst &ICI) {
  auto LHS = getMappedValue(ICI.getOperand(0));
  auto RHS = getMappedValue(ICI.getOperand(1));

  if (LHS.isScalar()) {
    ValueMap[&ICI] =
        Builder.CreateICmp(ICI.getPredicate(), LHS[0], RHS[0], ICI.getName());
    return;
  }

  WATEVER_UNIMPLEMENTED("icmp over 64-bit");
}

void FunctionLegalizer::visitFCmpInst(llvm::FCmpInst &FCI) {
  auto LHS = getMappedValue(FCI.getOperand(0));
  auto RHS = getMappedValue(FCI.getOperand(1));

  if (LHS.isScalar()) {
    switch (FCI.getPredicate()) {
    // These are natively supported
    case llvm::CmpInst::FCMP_FALSE:
    case llvm::CmpInst::FCMP_OEQ:
    case llvm::CmpInst::FCMP_OGT:
    case llvm::CmpInst::FCMP_OGE:
    case llvm::CmpInst::FCMP_OLT:
    case llvm::CmpInst::FCMP_OLE:
    case llvm::CmpInst::FCMP_UNE:
    case llvm::CmpInst::FCMP_TRUE:
      ValueMap[&FCI] =
          Builder.CreateFCmp(FCI.getPredicate(), LHS[0], RHS[0], FCI.getName());
      break;
    // These are not
    case llvm::CmpInst::FCMP_ONE: {
      auto *GT = Builder.CreateFCmpOGT(LHS[0], RHS[0]);
      auto *LT = Builder.CreateFCmpOLT(LHS[0], RHS[0]);
      ValueMap[&FCI] = Builder.CreateOr(GT, LT, FCI.getName());
      break;
    }
    case llvm::CmpInst::FCMP_ORD: {
      auto *FirstEQ = Builder.CreateFCmpOEQ(LHS[0], LHS[0]);
      auto *SecondEQ = Builder.CreateFCmpOEQ(RHS[0], RHS[0]);
      ValueMap[&FCI] = Builder.CreateAnd(FirstEQ, SecondEQ, FCI.getName());
      break;
    }
    case llvm::CmpInst::FCMP_UNO: {
      auto *FirstNE = Builder.CreateFCmpUNE(LHS[0], LHS[0]);
      auto *SecondNE = Builder.CreateFCmpUNE(RHS[0], RHS[0]);
      ValueMap[&FCI] = Builder.CreateOr(FirstNE, SecondNE);
      break;
    }
    case llvm::CmpInst::FCMP_UEQ: {
      auto *GT = Builder.CreateFCmpOGT(LHS[0], RHS[0]);
      auto *LT = Builder.CreateFCmpOLT(LHS[0], RHS[0]);
      auto *LTorGT = Builder.CreateOr(GT, LT);
      ValueMap[&FCI] = Builder.CreateXor(LTorGT, 1, FCI.getName());
      break;
    }
    case llvm::CmpInst::FCMP_UGT: {
      auto *LE = Builder.CreateFCmpOLE(LHS[0], RHS[0]);
      ValueMap[&FCI] = Builder.CreateXor(LE, 1, FCI.getName());
      break;
    }
    case llvm::CmpInst::FCMP_UGE: {
      auto *LT = Builder.CreateFCmpOLT(LHS[0], RHS[0]);
      ValueMap[&FCI] = Builder.CreateXor(LT, 1, FCI.getName());
      break;
    }
    case llvm::CmpInst::FCMP_ULT: {
      auto *GE = Builder.CreateFCmpOGE(LHS[0], RHS[0]);
      ValueMap[&FCI] = Builder.CreateXor(GE, 1, FCI.getName());
      break;
    }
    case llvm::CmpInst::FCMP_ULE: {
      auto *GT = Builder.CreateFCmpOGT(LHS[0], RHS[0]);
      ValueMap[&FCI] = Builder.CreateXor(GT, 1, FCI.getName());
      break;
    }
    default:
      WATEVER_UNREACHABLE("Illegal float comparison");
    }
    return;
  }

  WATEVER_UNIMPLEMENTED("vector fcmp");
}

void FunctionLegalizer::visitPHINode(llvm::PHINode &PN) {
  auto LegalTy = LegalizationPass::getLegalType(PN.getType());
  unsigned NumIncoming = PN.getNumIncomingValues();
  llvm::SmallVector<llvm::Value *, 2> NewPHIs;
  for (llvm::Type *PartTy : LegalTy) {
    auto *NewPHI = Builder.CreatePHI(PartTy, NumIncoming, PN.getName());
    NewPHIs.push_back(NewPHI);
  }
  ValueMap[&PN] = LegalValue(NewPHIs);
  // Needed, as we might not have all incoming values (e.g., if this is a loop
  // header)
  PHIsToFix.push_back(&PN);
}

void FunctionLegalizer::visitSelectInst(llvm::SelectInst &SI) {
  auto *Condition = getMappedValue(SI.getCondition())[0];

  auto True = getMappedValue(SI.getTrueValue());
  auto False = getMappedValue(SI.getFalseValue());

  // This will be optimized away, however, we have to ensure that the upper bits
  // are zeroed, as the i32 Condition might contain dirty bits
  Condition = Builder.CreateAnd(Condition, 1);
  Condition = Builder.CreateTrunc(Condition, Int1Ty);

  if (True.isScalar()) {
    ValueMap[&SI] =
        Builder.CreateSelect(Condition, True[0], False[0], SI.getName());
    return;
  }

  llvm::SmallVector<llvm::Value *, 2> Vs;

  for (auto [T, F] : llvm::zip_equal(True, False)) {
    Vs.push_back(Builder.CreateSelect(Condition, T, F));
  }
  ValueMap[&SI] = LegalValue(Vs);
}

void FunctionLegalizer::visitCallInst(llvm::CallInst &CI) {
  auto *OldCalledFunc = CI.getCalledFunction();

  // Handle intrinsics
  // They cannot have a legalized signature
  if (OldCalledFunc && OldCalledFunc->isIntrinsic()) {
    llvm::SmallVector<llvm::Value *> NewArgs;
    for (auto &Arg : CI.args()) {
      auto LegalArg = getMappedValue(Arg.get());
      if (!LegalArg.isScalar()) {
        WATEVER_UNIMPLEMENTED("reconstruct the original llvm::Value, based on "
                              "the legalized argument");
      }
      auto *ValueArg = LegalArg[0];
      // Value does not need preprocessing
      if (ValueArg->getType() == Arg->getType()) {
        NewArgs.push_back(ValueArg);
        continue;
      }

      if (ValueArg->getType()->isIntegerTy()) {
        NewArgs.push_back(Builder.CreateTrunc(ValueArg, Arg->getType()));
        continue;
      }

      WATEVER_UNIMPLEMENTED("unsupported delegalization of {}",
                            llvmToString(*ValueArg));
    }
    ValueMap[&CI] = LegalValue{Builder.CreateCall(OldCalledFunc, NewArgs)};
    return;
  }

  // Resolve callee
  llvm::Value *NewCallee = nullptr;
  // TODO For variadic functions, the call site might have a different type then
  // our call instructions
  if (!OldCalledFunc) {
    // Indirect call
    auto CalledOp = getMappedValue(CI.getCalledOperand());
    if (!CalledOp.isScalar()) {
      WATEVER_UNREACHABLE("indirectly called function is not scalar");
    }
    NewCallee = CalledOp[0];
  } else {
    // Direct call
    // Fallback to old function, in case of calling a declaration
    NewCallee = OldCalledFunc;
    if (auto It = FuncMap.find(OldCalledFunc); It != FuncMap.end()) {
      NewCallee = It->second;
    }
  }

  auto *NewFuncTy =
      LegalizationPass::getLegalFunctionType(CI.getFunctionType());

  llvm::SmallVector<llvm::Value *> NewArgs;
  // Handle indirect return value
  if (!LegalizationPass::getLegalType(CI.getType()).isScalar()) {
    // TODO maybe allocate in entry block, so we can use a static allocation
    NewArgs.push_back(Builder.CreateAlloca(CI.getType()));
  }

  // Fixed arguments
  uint32_t NumFixed = CI.getFunctionType()->getNumParams();
  uint32_t ArgIdx = 0;
  for (; ArgIdx < NumFixed; ++ArgIdx) {
    LegalValue LegalArgs = getMappedValue(CI.getArgOperand(ArgIdx));
    for (auto *Val : LegalArgs) {
      NewArgs.push_back(Val);
    }
  }

  // Variadic arguments
  if (CI.getFunctionType()->isVarArg()) {
    llvm::SmallVector<llvm::Value *> VarArgValues;
    llvm::SmallVector<llvm::Type *> VarArgTypes;

    for (; ArgIdx < CI.arg_size(); ++ArgIdx) {
      LegalValue LegalArgs = getMappedValue(CI.getArgOperand(ArgIdx));
      for (auto *Val : LegalArgs) {
        VarArgValues.push_back(Val);
        VarArgTypes.push_back(Val->getType());
      }
    }

    if (VarArgValues.empty()) {
      WATEVER_LOG_TRACE("vararg target {} with 0 variadic arguments",
                        CI.getFunction()->getName());
      NewArgs.push_back(llvm::Constant::getNullValue(PtrTy));
    } else {
      WATEVER_LOG_TRACE("vararg target {} with {} variadic arguments",
                        CI.getFunction()->getName(), VarArgTypes.size());
      llvm::StructType *VarArgsStructTy =
          llvm::StructType::get(Builder.getContext(), VarArgTypes);

      // Create static allocation in entry block
      llvm::Function *ParentFunc = Builder.GetInsertBlock()->getParent();
      llvm::IRBuilder<> EntryBuilder(&ParentFunc->getEntryBlock(),
                                     ParentFunc->getEntryBlock().begin());
      auto *VarArgsAlloca = EntryBuilder.CreateAlloca(VarArgsStructTy);

      // Store values
      auto &DL = CI.getModule()->getDataLayout();
      const llvm::StructLayout *Layout = DL.getStructLayout(VarArgsStructTy);
      llvm::Value *BaseInt = Builder.CreatePtrToInt(VarArgsAlloca, IntPtrTy);

      for (size_t I = 0; I < VarArgValues.size(); ++I) {
        uint64_t Offset = Layout->getElementOffset(I);
        auto *AddrInt = BaseInt;
        if (Offset != 0) {
          AddrInt = Builder.CreateAdd(BaseInt,
                                      llvm::ConstantInt::get(IntPtrTy, Offset));
        }
        llvm::Value *FieldPtr = Builder.CreateIntToPtr(AddrInt, PtrTy);
        Builder.CreateAlignedStore(VarArgValues[I], FieldPtr,
                                   DL.getABITypeAlign(VarArgTypes[I]));
      }

      NewArgs.push_back(VarArgsAlloca);
    }
  }

  assert(NewFuncTy->getNumParams() == NewArgs.size() &&
         "Argument count mismatch!");

  auto *NewCall =
      Builder.CreateCall(NewFuncTy, NewCallee, NewArgs, CI.getName());
  NewCall->setCallingConv(CI.getCallingConv());

  if (CI.getAttributes().hasFnAttrs()) {
    NewCall->setAttributes(llvm::AttributeList::get(
        NewCall->getContext(), llvm::AttributeList::FunctionIndex,
        CI.getAttributes().getFnAttrs()));
  }

  ValueMap[&CI] = LegalValue{NewCall};
}

llvm::Function *LegalizationPass::createLegalFunction(llvm::Module &Mod,
                                                      llvm::Function *OldFunc) {
  llvm::Function *Fn =
      llvm::Function::Create(getLegalFunctionType(OldFunc->getFunctionType()),
                             OldFunc->getLinkage(), OldFunc->getName(), Mod);

  // Copy all attributes of the function
  // TODO maybe handle parametere attributes too - however, they cannot just be
  // copied, as legalization potentially changes the signature
  llvm::AttributeSet FnAttrs = OldFunc->getAttributes().getFnAttrs();
  Fn->setAttributes(llvm::AttributeList::get(
      Fn->getContext(), llvm::AttributeList::FunctionIndex, FnAttrs));

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
  if (OldFunc->getFunctionType()->isVarArg()) {
    Fn->getArg(I)->setName("varargs");
  }

  return Fn;
}

llvm::FunctionType *
LegalizationPass::getLegalFunctionType(llvm::FunctionType *OldFuncTy) {
  auto LegalResultTy = getLegalType(OldFuncTy->getReturnType());

  llvm::Type *ResultTy = nullptr;
  llvm::SmallVector<llvm::Type *> Params;
  if (LegalResultTy.size() > 1) {
    // TODO the spec uses multi value return types, however, LLVM still
    // compiles with indirect types
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

  // Pointer to the arguments
  if (OldFuncTy->isVarArg()) {
    Params.push_back(llvm::PointerType::get(OldFuncTy->getContext(), 0));
  }

  return llvm::FunctionType::get(ResultTy, Params, false);
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

  // Old Funcs -> New Funcs
  llvm::DenseMap<llvm::Function *, llvm::Function *> FuncMap;
  for (auto &F : Mod) {
    if (F.isIntrinsic()) {
      continue;
    }
    FuncsToLegalize.push_back(&F);
  }

  // Generate mapping, without bodies
  for (auto *F : FuncsToLegalize) {
    auto *NewFunc = LegalizationPass::createLegalFunction(Mod, F);
    NewFunc->takeName(F);
    FuncMap[F] = NewFunc;
  }

  llvm::IRBuilder<> Builder(Mod.getContext());
  for (auto *F : FuncsToLegalize) {
    if (F->isDeclaration())
      continue;
    WATEVER_LOG_DBG("Legalizing {}", F->getName().str());
    auto *NewFunc = FuncMap[F];
    FunctionLegalizer FL{F, NewFunc, Builder, Config, FuncMap};
    FL.visit(F);
    FL.fixupPHIs();
    WATEVER_LOG_DBG("Legalized Function:\n {}", llvmToString(*FL.NewFunc));
  }

  for (auto *F : FuncsToLegalize) {
    auto *NewFunc = FuncMap[F];
    F->replaceAllUsesWith(NewFunc);
    F->eraseFromParent();
  }

  return llvm::PreservedAnalyses::none();
}
