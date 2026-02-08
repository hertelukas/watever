#include "watever/legalization.hpp"
#include "watever/utils.hpp"
#include <algorithm>
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
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Operator.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/Alignment.h>
#include <llvm/Support/Casting.h>

using namespace watever;

LegalValue FunctionLegalizer::legalizeConstant(llvm::Constant *C) {
  if (auto *CI = llvm::dyn_cast<llvm::ConstantInt>(C)) {
    const unsigned Width = CI->getBitWidth();
    if (Width == 32 || Width == 64) {
      return C;
    }
    if (Width < 32) {
      return llvm::ConstantInt::get(Builder.getInt32Ty(), CI->getSExtValue());
    }
    if (Width < 64) {
      return llvm::ConstantInt::get(Builder.getInt64Ty(), CI->getSExtValue());
    }

    unsigned NumParts = (Width + 63) / 64;
    auto Value = CI->getValue();

    llvm::SmallVector<llvm::Value *> Vs;
    for (unsigned I = 0; I < NumParts - 1; ++I) {
      Vs.push_back(llvm::ConstantInt::get(
          Builder.getInt64Ty(), Value.extractBitsAsZExtValue(64, I * 64)));
    }
    // Handle last
    unsigned LastNumBits = Width % 64;
    LastNumBits = LastNumBits == 0 ? 64 : LastNumBits;
    Vs.push_back(llvm::ConstantInt::get(
        Builder.getInt64Ty(),
        Value.extractBitsAsZExtValue(LastNumBits, (NumParts - 1) * 64)));

    return LegalValue{Vs};
  }

  if (auto *CE = llvm::dyn_cast<llvm::ConstantExpr>(C)) {
    if (auto *GEP = llvm::dyn_cast<llvm::GEPOperator>(CE)) {
      if (auto *PointerConstant =
              llvm::dyn_cast<llvm::Constant>(GEP->getPointerOperand())) {
        const llvm::DataLayout &DL = NewFunc->getParent()->getDataLayout();
        llvm::APInt Offset(
            DL.getPointerSizeInBits(GEP->getPointerAddressSpace()), 0);
        auto Base = legalizeConstant(PointerConstant);
        assert(Base.isScalar() &&
               "pointer base in constant GEP must be scalar");
        if (GEP->accumulateConstantOffset(DL, Offset)) {
          WATEVER_LOG_TRACE("GEP with constant offset of {}",
                            Offset.getSExtValue());
          if (Offset.isZero()) {
            return Base[0];
          }
          llvm::Value *OffsetVal = llvm::ConstantInt::get(IntPtrTy, Offset);

          // Forcve creation of instructions
          llvm::Value *PtrAsInt = llvm::CastInst::Create(
              llvm::Instruction::PtrToInt, Base[0], IntPtrTy);
          Builder.Insert(PtrAsInt);

          llvm::Value *PtrWithOffsetAsInt =
              llvm::BinaryOperator::CreateAdd(PtrAsInt, OffsetVal);
          Builder.Insert(PtrWithOffsetAsInt);

          llvm::Value *PtrWithOffset = llvm::CastInst::Create(
              llvm::Instruction::IntToPtr, PtrWithOffsetAsInt, PtrTy);
          Builder.Insert(PtrWithOffset);

          return LegalValue{PtrWithOffset};
        }
        WATEVER_UNREACHABLE("constant GEP is not able to accumulate offset");
      }
      WATEVER_UNREACHABLE(
          "constant GEP does not have a constant as pointer operand");
    }
    if (auto *PtrToInt = llvm::dyn_cast<llvm::PtrToIntOperator>(CE)) {
      if (auto *PointerConstant =
              llvm::dyn_cast<llvm::Constant>(PtrToInt->getPointerOperand())) {
        auto LegalPtr = legalizeConstant(PointerConstant);
        assert(LegalPtr.isScalar() && "pointer in ptrtoint must be scalar");

        auto *Inst = llvm::CastInst::Create(llvm::Instruction::PtrToInt,
                                            LegalPtr[0], CE->getType());
        Builder.Insert(Inst);
        return LegalValue{Inst};
      }
      WATEVER_UNREACHABLE(
          "constant ptrtoint does not have a constant as pointer operand");
    }
    if (CE->getOpcode() == llvm::Instruction::IntToPtr) {
      if (auto *IntConstant =
              llvm::dyn_cast<llvm::Constant>(CE->getOperand(0))) {
        auto LegalInt = legalizeConstant(IntConstant);
        assert(LegalInt.isScalar() && "int in inttoptr must be scalar");

        auto *Inst = llvm::CastInst::Create(llvm::Instruction::IntToPtr,
                                            LegalInt[0], CE->getType());
        Builder.Insert(Inst);
        return LegalValue{Inst};
      }
      WATEVER_UNREACHABLE(
          "constant inttoptr does not have a constant as int operand");
    }
    WATEVER_UNIMPLEMENTED("constant expression {}", llvmToString(*CE));
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

  if (auto *CA = llvm::dyn_cast<llvm::ConstantAggregate>(C)) {
    llvm::SmallVector<llvm::Value *> Res;
    for (unsigned I = 0; I < CA->getNumOperands(); ++I) {
      auto Elm = legalizeConstant(CA->getOperand(I));
      Res.append(Elm.begin(), Elm.end());
    }
    return LegalValue{Res};
  }

  if (auto *CAZ = llvm::dyn_cast<llvm::ConstantAggregateZero>(C)) {
    llvm::SmallVector<llvm::Value *> Res;
    if (auto *AT = llvm::dyn_cast<llvm::ArrayType>(CAZ->getType())) {
      auto Element =
          legalizeConstant(llvm::Constant::getNullValue(AT->getElementType()));
      for (unsigned I = 0; I < AT->getNumElements(); ++I) {
        Res.append(Element.begin(), Element.end());
      }
    } else if (auto *ST = llvm::dyn_cast<llvm::StructType>(CAZ->getType())) {
      for (unsigned I = 0; I < ST->getNumElements(); ++I) {
        auto Element = legalizeConstant(
            llvm::Constant::getNullValue(ST->getElementType(I)));
        Res.append(Element.begin(), Element.end());
      }
    }
    return LegalValue{Res};
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

void FunctionLegalizer::generateClusters(llvm::SwitchInst &SI,
                                         PartitionList &PL) {
  if (SI.getCondition()->getType()->getIntegerBitWidth() > 64) {
    WATEVER_UNIMPLEMENTED(
        "Only switch cases up to i64 are supported (due to sext)");
  }

  auto N = SI.getNumCases();
  // How large a single generated jump tables can get - used to ensure that
  // algorithm runs in O(nlogn)
  unsigned Max = 256;
  // Minimal density for branch tables.
  float D = 0.2;

  llvm::SmallVector<llvm::SwitchInst::CaseHandle> SortedCases;
  for (auto &Case : SI.cases()) {
    SortedCases.push_back(Case);
  }

  std::ranges::sort(SortedCases, [](const llvm::SwitchInst::CaseHandle &A,
                                    const llvm::SwitchInst::CaseHandle &B) {
    return A.getCaseValue()->getSExtValue() < B.getCaseValue()->getSExtValue();
  });

  llvm::SmallVector<uint32_t> MinP(N);
  llvm::SmallVector<uint32_t> LastElement(N);

  MinP[N - 1] = 1;
  LastElement[N - 1] = N - 1;

  auto Density = [&](uint32_t i, uint32_t j) {
    return static_cast<float>(j - i + 1) /
           (SortedCases[j].getCaseValue()->getSExtValue() -
            SortedCases[i].getCaseValue()->getSExtValue() + 1);
  };

  for (unsigned i = N - 2; i < N; --i) {
    MinP[i] = 1 + MinP[i + 1];
    LastElement[i] = i;

    for (unsigned j = std::min(N - 1, i + Max); j > i; --j) {
      int64_t L = SortedCases[j].getCaseValue()->getSExtValue() -
                  SortedCases[i].getCaseValue()->getSExtValue();
      if (L <= Max && D < Density(i, j)) {
        uint32_t NumParts = j == N - 1 ? 1 : 1 + MinP[j + 1];
        if (NumParts < MinP[i]) {
          MinP[i] = NumParts;
          LastElement[i] = j;
        }
      }
    }
  }

  unsigned i = 0;
  unsigned j;
  while (i < N) {
    j = LastElement[i];
    auto C = Cluster(SortedCases[i].getCaseValue()->getSExtValue(),
                     SortedCases[j].getCaseValue()->getSExtValue());
    WATEVER_LOG_TRACE("New cluster from {} to {}", C.Min, C.Max);
    for (unsigned k = i; k <= j; ++k) {
      C.Cases.push_back(SortedCases[k]);
    }
    PL.push_back(std::move(C));
    i = j + 1;
  }
}

void FunctionLegalizer::visitSwitchInst(llvm::SwitchInst &SI) {
  auto Cond = getMappedValue(SI.getCondition());
  if (!Cond.isScalar()) {
    WATEVER_UNIMPLEMENTED("switch on non-scalar value");
  }

  auto *CondVal = Cond[0];
  if (SI.getCondition()->getType() != CondVal->getType()) {
    CondVal = Builder.CreateAnd(
        CondVal, llvm::APInt::getLowBitsSet(
                     32, SI.getCondition()->getType()->getIntegerBitWidth()));
  }
  auto *DefaultDest =
      llvm::dyn_cast<llvm::BasicBlock>(getMappedValue(SI.getDefaultDest())[0]);

  PartitionList PL{};
  generateClusters(SI, PL);

  if (PL.empty()) {
    Builder.CreateBr(DefaultDest);
    return;
  }

  // Build binary search tree
  // Emits a tree for the partitions between Begin and End
  auto EmitTree = [&](auto &&Self, size_t Begin, size_t End, int64_t LowerBound,
                      int64_t UpperBound) {
    // Leaf node (one partition)
    if (Begin == End) {
      const auto &C = PL[Begin];
      // If only one case, conditional branch
      if (C.Cases.size() == 1) {
        // Needed, as CondVal might be a legalized version of the original
        // switch
        auto *CaseVal = llvm::ConstantInt::get(
            CondVal->getType(), C.Cases[0].getCaseValue()->getSExtValue());
        auto *Dest = llvm::dyn_cast<llvm::BasicBlock>(
            getMappedValue(C.Cases[0].getCaseSuccessor())[0]);
        auto *IsEq = Builder.CreateICmpEQ(CondVal, CaseVal);
        Builder.CreateCondBr(IsEq, Dest, DefaultDest);
        return;
      }
      // Cluster, emit (shifted) branch table (switch)
      auto *SwitchCond = CondVal;
      if (C.Min != 0) {
        auto *MinConst = llvm::ConstantInt::get(CondVal->getType(), C.Min);
        SwitchCond = Builder.CreateSub(CondVal, MinConst);
      }
      // Branch tables have a max. size of 256 and are 0 normalized, so the
      // condition will always fit into 32-bit here. However, in a 64-bit
      // condition, the value might overflow
      if (SwitchCond->getType()->getIntegerBitWidth() != 32) {
        assert(SwitchCond->getType()->getIntegerBitWidth() == 64 &&
               "bit width must be 32-bit or 64");
        // If the possible values with which we reach this leaf are > 2^32 apart
        // form each other, we need to manually branch to default, as a
        // truncation will wrap around
        if ((uint64_t)(UpperBound - LowerBound) > 0xFF'FF'FF'FFULL) {
          uint64_t Range = C.Max - C.Min;
          auto *InBounds = Builder.CreateICmpULE(
              SwitchCond, llvm::ConstantInt::get(SwitchCond->getType(), Range));
          auto *SwitchBB = llvm::BasicBlock::Create(NewFunc->getContext(),
                                                    "sw.cluster", NewFunc);
          Builder.CreateCondBr(InBounds, SwitchBB, DefaultDest);
          Builder.SetInsertPoint(SwitchBB);
        }
        SwitchCond = Builder.CreateTrunc(SwitchCond, Int32Ty);
      }
      auto *SubSwitch =
          Builder.CreateSwitch(SwitchCond, DefaultDest, C.Cases.size());
      for (const auto &Case : C.Cases) {
        auto *Val = llvm::cast<llvm::ConstantInt>(llvm::ConstantInt::get(
            Int32Ty, Case.getCaseValue()->getSExtValue() - C.Min));
        auto *Dest = llvm::dyn_cast<llvm::BasicBlock>(
            getMappedValue(Case.getCaseSuccessor())[0]);
        SubSwitch->addCase(Val, Dest);
      }
      return;
    }

    // Recurse
    size_t Mid = Begin + (End - Begin) / 2;
    int64_t PivotVal = PL[Mid].Max;

    auto *LeftBB =
        llvm::BasicBlock::Create(NewFunc->getContext(), "sw.left", NewFunc);
    auto *RightBB =
        llvm::BasicBlock::Create(NewFunc->getContext(), "sw.right", NewFunc);

    auto *Pivot = llvm::ConstantInt::get(CondVal->getType(), PivotVal);
    auto *Cmp = Builder.CreateICmpSLE(CondVal, Pivot, "sw.pivot");
    Builder.CreateCondBr(Cmp, LeftBB, RightBB);

    Builder.SetInsertPoint(LeftBB);
    Self(Self, Begin, Mid, LowerBound, PivotVal);

    Builder.SetInsertPoint(RightBB);
    Self(Self, Mid + 1, End, PivotVal + 1, UpperBound);
  };

  EmitTree(EmitTree, 0, PL.size() - 1, INT64_MIN, INT64_MAX);
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
      if (LHS->getType()->isDoubleTy()) {
        ValueMap[&BO] = emitLibCall("fmod", {LHS, RHS}, LHS->getType());
      } else if (LHS->getType()->isFloatTy()) {
        ValueMap[&BO] = emitLibCall("fmodf", {LHS, RHS}, LHS->getType());
      } else {
        WATEVER_UNREACHABLE("Unsupported type for frem {}",
                            llvmToString(*LHS->getType()));
      }
      return;
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
void FunctionLegalizer::visitExtractValueInst(llvm::ExtractValueInst &EI) {
  auto LegalAggregateOperand = getMappedValue(EI.getAggregateOperand());
  LegalValue Result;
  const auto *OpIt = LegalAggregateOperand.begin();

  auto RecursiveExtract = [&](auto &&Self, llvm::Type *Ty,
                              const unsigned int *IdxIt) {
    if (IdxIt == EI.idx_end()) {
      // Collect all scalar parts of the type
      auto NumParts = LegalizationPass::getLegalType(Ty).size();
      for (size_t I = 0; I < NumParts; ++I) {
        Result.PushBack(*OpIt++);
      }
      return;
    }

    unsigned TargetIdx = *IdxIt;

    if (auto *STy = llvm::dyn_cast<llvm::StructType>(Ty)) {
      for (unsigned I = 0; I < TargetIdx; ++I) {
        // Skip the type's value in the input operand
        auto NumParts =
            LegalizationPass::getLegalType(STy->getElementType(I)).size();
        OpIt += NumParts;
      }
      Self(Self, STy->getElementType(TargetIdx), IdxIt + 1);
    } else if (auto *ATy = llvm::dyn_cast<llvm::ArrayType>(Ty)) {
      auto *ElemTy = ATy->getElementType();
      auto ElementSize = LegalizationPass::getLegalType(ElemTy).size();
      OpIt += (ElementSize * TargetIdx);
      Self(Self, ElemTy, IdxIt + 1);
    } else {
      WATEVER_UNREACHABLE("index into non-aggregate type");
    }
  };

  RecursiveExtract(RecursiveExtract, EI.getAggregateOperand()->getType(),
                   EI.idx_begin());

  ValueMap[&EI] = Result;
}
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

llvm::Value *FunctionLegalizer::emitScalarLoad(llvm::Value *Ptr,
                                               llvm::Type *ResultType,
                                               llvm::Align Align) {

  if (ResultType->isIntegerTy()) {
    const unsigned Width = ResultType->getIntegerBitWidth();

    // We can just load the int
    if (Width == 32 || Width == 64) {
      return Builder.CreateAlignedLoad(ResultType, Ptr, Align);
    }
    if (Width > 64) {
      WATEVER_UNIMPLEMENTED("expanding load not supported");
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
      llvm::Value *Result = Builder.CreateAlignedLoad(TypeToLoad, Ptr, Align);

      if (TypeToLoad != TargetType) {
        Result = Builder.CreateZExt(Result, TargetType);
      }
      return Result;
    }

    // Use multiple loads as near around the requested width (E.g., a load i56
    // is a load 32, load 16, load 8)
    llvm::Value *Result;
    // We want to build an i64 with the result
    if (Width > 32) {
      Result = Builder.CreateLoad(Int32Ty, Ptr);
      Result = Builder.CreateZExt(Result, Int64Ty);
      auto *PtrAsInt = Builder.CreatePtrToInt(Ptr, IntPtrTy);
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
      Result = Builder.CreateLoad(Int16Ty, Ptr);
      Result = Builder.CreateZExt(Result, Int32Ty);

      // Load the next 8 bit; add two to the pointer
      llvm::Value *NextOffset = llvm::ConstantInt::get(IntPtrTy, 2);
      llvm::Value *PtrAsInt = Builder.CreatePtrToInt(Ptr, IntPtrTy);
      llvm::Value *NewPtrAsInt = Builder.CreateAdd(PtrAsInt, NextOffset);
      Ptr = Builder.CreateIntToPtr(NewPtrAsInt, PtrTy);

      llvm::Value *NextByte = Builder.CreateLoad(Int8Ty, Ptr);
      NextByte = Builder.CreateZExt(NextByte, Int32Ty);
      NextByte = Builder.CreateShl(NextByte, 16);
      Result = Builder.CreateOr(Result, NextByte);
    }
    return Result;
  }
  if (ResultType->isFloatingPointTy()) {
    if (ResultType->isDoubleTy() || ResultType->isFloatTy()) {
      return Builder.CreateAlignedLoad(ResultType, Ptr, Align);
    }
    WATEVER_UNIMPLEMENTED("handle load of unsupported floating point type {}",
                          llvmToString(*ResultType));
  }
  if (ResultType->isPointerTy()) {
    return Builder.CreateAlignedLoad(PtrTy, Ptr, Align);
  }

  WATEVER_UNIMPLEMENTED("scalar load of type {}", llvmToString(*ResultType));
}

void FunctionLegalizer::visitLoadInst(llvm::LoadInst &LI) {
  auto *ResultType = LI.getType();
  auto *BasePtr = getMappedValue(LI.getPointerOperand())[0];
  LegalValue Result;

  auto &DL = LI.getDataLayout();
  const auto BaseAlign = LI.getAlign();

  auto RecursiveLoad = [&](auto &&Self, llvm::Type *Ty,
                           uint64_t CurrentOffset) {
    if (!Ty->isAggregateType()) {
      auto CurrentAlign = llvm::commonAlignment(BaseAlign, CurrentOffset);
      llvm::Value *Ptr = BasePtr;
      if (CurrentOffset != 0) {
        Ptr =
            llvm::CastInst::Create(llvm::Instruction::PtrToInt, Ptr, IntPtrTy);
        Builder.Insert(Ptr);

        Ptr = llvm::BinaryOperator::CreateAdd(
            Ptr, llvm::ConstantInt::get(IntPtrTy, CurrentOffset));
        Builder.Insert(Ptr);

        Ptr = llvm::CastInst::Create(llvm::Instruction::IntToPtr, Ptr, PtrTy);
        Builder.Insert(Ptr);
      }
      Result.PushBack(emitScalarLoad(Ptr, Ty, CurrentAlign));
      return;
    }
    if (auto *STy = llvm::dyn_cast<llvm::StructType>(Ty)) {
      const llvm::StructLayout *Layout = DL.getStructLayout(STy);
      for (unsigned I = 0; I < STy->getNumElements(); ++I) {
        auto Offset = Layout->getElementOffset(I);
        Self(Self, STy->getElementType(I), CurrentOffset + Offset);
      }
    } else if (auto *ATy = llvm::dyn_cast<llvm::ArrayType>(Ty)) {
      auto *ElementType = ATy->getElementType();
      auto Size = DL.getTypeAllocSize(ElementType);
      for (unsigned I = 0; I < ATy->getNumElements(); ++I) {
        Self(Self, ElementType, CurrentOffset + I * Size);
      }
    }
  };

  RecursiveLoad(RecursiveLoad, ResultType, 0);
  ValueMap[&LI] = Result;
}

void FunctionLegalizer::emitScalarStore(llvm::Value *Val, llvm::Value *Ptr,
                                        llvm::Type *StoreType,
                                        llvm::Align Align) {
  if (StoreType->isIntegerTy()) {
    const unsigned Width = StoreType->getIntegerBitWidth();
    if (Width == 32 || Width == 64) {
      Builder.CreateAlignedStore(Val, Ptr, Align);
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
          Val, llvm::APInt::getLowBitsSet(Val->getType()->getIntegerBitWidth(),
                                          Width));
    } else {
      ToStore = Val;
    }

    // Can't split
    if (Width > 32 + 16 + 8 || (Width < 32 && Width > 16 + 8)) {
      Builder.CreateAlignedStore(ToStore, Ptr, Align);
      return;
    }

    if (Width > 32) {
      llvm::Value *LowestBytes = Builder.CreateTrunc(ToStore, Int32Ty);
      Builder.CreateStore(LowestBytes, Ptr);

      llvm::Value *PtrAsInt = Builder.CreatePtrToInt(Ptr, IntPtrTy);

      unsigned BytesStored = 4;
      // The number is wide enough to require a 16-bit store
      if (Width > 32 + 8) {
        llvm::Value *NewPtrAsInt = Builder.CreateAdd(
            PtrAsInt, llvm::ConstantInt::get(IntPtrTy, BytesStored));
        llvm::Value *NewPtr = Builder.CreateIntToPtr(NewPtrAsInt, PtrTy);
        llvm::Value *NextBytes = Builder.CreateLShr(ToStore, BytesStored * 8);
        NextBytes = Builder.CreateTrunc(NextBytes, Int16Ty);
        Builder.CreateStore(NextBytes, NewPtr);
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
        Builder.CreateStore(NextByte, NewPtr);
      }
    } else {
      // We need to store at least a 16-bit
      if (Width > 8) {
        llvm::Value *LowestBytes = Builder.CreateTrunc(ToStore, Int16Ty);
        Builder.CreateStore(LowestBytes, Ptr);
      }
      if (Width <= 8 || Width > 16) {
        // We are storing higher bytes
        if (Width > 16) {
          ToStore = Builder.CreateLShr(ToStore, 16);
          llvm::Value *IntPtr = Builder.CreatePtrToInt(Ptr, IntPtrTy);
          IntPtr =
              Builder.CreateAdd(IntPtr, llvm::ConstantInt::get(IntPtrTy, 2));
          Ptr = Builder.CreateIntToPtr(IntPtr, PtrTy);
        }
        llvm::Value *LowestByte = Builder.CreateTrunc(ToStore, Int8Ty);
        Builder.CreateStore(LowestByte, Ptr);
      }
    }
    return;
  }
  if (StoreType->isDoubleTy() || StoreType->isFloatTy()) {
    Builder.CreateAlignedStore(Val, Ptr, Align);
    return;
  }

  if (StoreType->isPointerTy()) {
    Builder.CreateAlignedStore(Val, Ptr, Align);
    return;
  }

  WATEVER_UNIMPLEMENTED("scalar store of type {}", llvmToString(*StoreType));
}

void FunctionLegalizer::visitStoreInst(llvm::StoreInst &SI) {
  auto *StoreType = SI.getValueOperand()->getType();
  auto StoreValues = getMappedValue(SI.getValueOperand());
  auto *BasePtr = getMappedValue(SI.getPointerOperand())[0];

  auto &DL = SI.getDataLayout();
  const auto BaseAlign = SI.getAlign();

  auto RecursiveStore = [&](auto &&Self, llvm::Type *Ty, uint64_t CurrentOffset,
                            LegalValue::iterator &ValIt) {
    if (!Ty->isAggregateType()) {
      auto CurrentAlign = llvm::commonAlignment(BaseAlign, CurrentOffset);
      llvm::Value *Ptr = BasePtr;
      if (CurrentOffset != 0) {
        Ptr = Builder.CreatePtrToInt(Ptr, IntPtrTy);
        Ptr = Builder.CreateAdd(
            Ptr, llvm::ConstantInt::get(IntPtrTy, CurrentOffset));
        Ptr = Builder.CreateIntToPtr(Ptr, PtrTy);
      }
      emitScalarStore(*ValIt, Ptr, Ty, CurrentAlign);
      ++ValIt;
      return;
    }

    if (auto *STy = llvm::dyn_cast<llvm::StructType>(Ty)) {
      const llvm::StructLayout *Layout = DL.getStructLayout(STy);
      for (unsigned I = 0; I < STy->getNumElements(); ++I) {
        auto Offset = Layout->getElementOffset(I);
        Self(Self, STy->getElementType(I), CurrentOffset + Offset, ValIt);
      }
    } else if (auto *ATy = llvm::dyn_cast<llvm::ArrayType>(Ty)) {
      auto *ElementType = ATy->getElementType();
      auto Size = DL.getTypeAllocSize(ElementType);
      for (unsigned I = 0; I < ATy->getNumElements(); ++I) {
        Self(Self, ElementType, CurrentOffset + I * Size, ValIt);
      }
    }
  };
  const auto *It = StoreValues.begin();
  RecursiveStore(RecursiveStore, StoreType, 0, It);
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

  if (FirstTypeSize == 1) {
    TotalOffset = SextIdx;
  } else if (FirstTypeSize != 0) {
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

      if (FieldOffset != 0) {
        TotalOffset = Builder.CreateAdd(
            TotalOffset, llvm::ConstantInt::get(IntPtrTy, FieldOffset));
      }
      CurrentTy = STy->getElementType(FieldIdx);
    } else {
      CurrentTy = CurrentTy->getContainedType(0);
      uint64_t ElementSize = DL.getTypeAllocSize(CurrentTy);

      WATEVER_LOG_TRACE("Accessing array at index {}, with element sizes of {}",
                        llvmToString(*SextIdx), ElementSize);

      llvm::Value *ScaledOffset = SextIdx;
      if (ElementSize != 1) {
        ScaledOffset = Builder.CreateMul(
            SextIdx, llvm::ConstantInt::get(IntPtrTy, ElementSize));
      }
      TotalOffset = Builder.CreateAdd(TotalOffset, ScaledOffset);
    }
  }
  llvm::Value *BasePtrInt = nullptr;
  if (llvm::isa<llvm::Constant>(PtrArg)) {
    // Prevent inlining
    auto *Inst = llvm::CastInst::Create(llvm::Instruction::PtrToInt, PtrArg,
                                        IntPtrTy, "base_int");
    BasePtrInt = Builder.Insert(Inst);
  } else {
    BasePtrInt = Builder.CreatePtrToInt(PtrArg, IntPtrTy, "base_int");
  }
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
    // If we currently filled only parts of a Wasm value, we have to ensure
    // that the upper bits in the new value are clean
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

void FunctionLegalizer::visitFPToUIInst(llvm::FPToUIInst &FI) {
  auto Arg = getMappedValue(FI.getOperand(0));

  if (Arg.isScalar()) {
    auto TargetWidth = FI.getDestTy()->getIntegerBitWidth();

    if (FI.getSrcTy()->isFloatTy() || FI.getSrcTy()->isDoubleTy()) {
      if (TargetWidth <= 32) {
        ValueMap[&FI] = Builder.CreateFPToUI(Arg[0], Int32Ty);
      } else if (TargetWidth <= 64) {
        ValueMap[&FI] = Builder.CreateFPToUI(Arg[0], Int64Ty);
      }
      return;
    }
  }
  WATEVER_UNIMPLEMENTED("fp to unsigned integer from {} to {}",
                        llvmToString(*FI.getSrcTy()),
                        llvmToString(*FI.getDestTy()));
}

void FunctionLegalizer::visitFPToSIInst(llvm::FPToSIInst &FI) {
  auto Arg = getMappedValue(FI.getOperand(0));

  if (Arg.isScalar()) {
    auto TargetWidth = FI.getDestTy()->getIntegerBitWidth();

    if (FI.getSrcTy()->isFloatTy() || FI.getSrcTy()->isDoubleTy()) {
      if (TargetWidth <= 32) {
        ValueMap[&FI] = Builder.CreateFPToSI(Arg[0], Int32Ty);
      } else if (TargetWidth <= 64) {
        ValueMap[&FI] = Builder.CreateFPToSI(Arg[0], Int64Ty);
      }
      return;
    }
  }
  WATEVER_UNIMPLEMENTED("fp to unsigned integer from {} to {}",
                        llvmToString(*FI.getSrcTy()),
                        llvmToString(*FI.getDestTy()));
}

void FunctionLegalizer::visitUIToFPInst(llvm::UIToFPInst &UI) {
  auto Arg = getMappedValue(UI.getOperand(0));
  auto *TargetTy = UI.getDestTy();

  if (!TargetTy->isDoubleTy() && !TargetTy->isFloatingPointTy()) {
    WATEVER_UNIMPLEMENTED("unsupported UI to FP type {}",
                          llvmToString(*TargetTy));
  }

  if (Arg.isScalar()) {
    auto *Extended = zeroExtend(Arg[0], UI.getSrcTy()->getIntegerBitWidth(),
                                Arg[0]->getType()->getIntegerBitWidth());
    ValueMap[&UI] = Builder.CreateUIToFP(Extended, TargetTy);
    return;
  }
  WATEVER_UNIMPLEMENTED("non-scalar UI {} to FP {}",
                        llvmToString(*UI.getSrcTy()),
                        llvmToString(*UI.getDestTy()));
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
    auto *Comparison =
        Builder.CreateICmp(ICI.getPredicate(), LHS[0], RHS[0], ICI.getName());

    ValueMap[&ICI] = Builder.CreateZExt(Comparison, Int32Ty);
    return;
  }

  WATEVER_UNIMPLEMENTED("icmp over 64-bit");
}

void FunctionLegalizer::visitFCmpInst(llvm::FCmpInst &FCI) {
  auto LHS = getMappedValue(FCI.getOperand(0));
  auto RHS = getMappedValue(FCI.getOperand(1));
  llvm::Value *Comparison;

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
      Comparison =
          Builder.CreateFCmp(FCI.getPredicate(), LHS[0], RHS[0], FCI.getName());
      break;
    // These are not
    case llvm::CmpInst::FCMP_ONE: {
      auto *GT = Builder.CreateFCmpOGT(LHS[0], RHS[0]);
      auto *LT = Builder.CreateFCmpOLT(LHS[0], RHS[0]);
      Comparison = Builder.CreateOr(GT, LT, FCI.getName());
      break;
    }
    case llvm::CmpInst::FCMP_ORD: {
      auto *FirstEQ = Builder.CreateFCmpOEQ(LHS[0], LHS[0]);
      auto *SecondEQ = Builder.CreateFCmpOEQ(RHS[0], RHS[0]);
      Comparison = Builder.CreateAnd(FirstEQ, SecondEQ, FCI.getName());
      break;
    }
    case llvm::CmpInst::FCMP_UNO: {
      auto *FirstNE = Builder.CreateFCmpUNE(LHS[0], LHS[0]);
      auto *SecondNE = Builder.CreateFCmpUNE(RHS[0], RHS[0]);
      Comparison = Builder.CreateOr(FirstNE, SecondNE);
      break;
    }
    case llvm::CmpInst::FCMP_UEQ: {
      auto *GT = Builder.CreateFCmpOGT(LHS[0], RHS[0]);
      auto *LT = Builder.CreateFCmpOLT(LHS[0], RHS[0]);
      auto *LTorGT = Builder.CreateOr(GT, LT);
      Comparison = Builder.CreateXor(LTorGT, 1, FCI.getName());
      break;
    }
    case llvm::CmpInst::FCMP_UGT: {
      auto *LE = Builder.CreateFCmpOLE(LHS[0], RHS[0]);
      Comparison = Builder.CreateXor(LE, 1, FCI.getName());
      break;
    }
    case llvm::CmpInst::FCMP_UGE: {
      auto *LT = Builder.CreateFCmpOLT(LHS[0], RHS[0]);
      Comparison = Builder.CreateXor(LT, 1, FCI.getName());
      break;
    }
    case llvm::CmpInst::FCMP_ULT: {
      auto *GE = Builder.CreateFCmpOGE(LHS[0], RHS[0]);
      Comparison = Builder.CreateXor(GE, 1, FCI.getName());
      break;
    }
    case llvm::CmpInst::FCMP_ULE: {
      auto *GT = Builder.CreateFCmpOGT(LHS[0], RHS[0]);
      Comparison = Builder.CreateXor(GT, 1, FCI.getName());
      break;
    }
    default:
      WATEVER_UNREACHABLE("Illegal float comparison");
    }
    ValueMap[&FCI] = Builder.CreateZExt(Comparison, Int32Ty, FCI.getName());
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

  // This will be optimized away, however, we have to ensure that the upper
  // bits are zeroed, as the i32 Condition might contain dirty bits
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

void FunctionLegalizer::visitFreezeInst(llvm::FreezeInst &FI) {
  ValueMap[&FI] = getMappedValue(FI.getOperand(0));
}

void FunctionLegalizer::visitCallInst(llvm::CallInst &CI) {
  auto *OldCalledFunc = CI.getCalledFunction();

  // Resolve callee
  llvm::Value *NewCallee = nullptr;
  // TODO For variadic functions, the call site might have a different type
  // then our call instructions
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

//===----------------------------------------------------------------------===//
// Intrinsics
//===----------------------------------------------------------------------===//

void FunctionLegalizer::visitIntrinsicInst(llvm::IntrinsicInst &II) {
  // Most intrinsics do not benefit from living until lowering. All of them
  // are handled in the following switch

  // Functions taking exactly a float/double and returning a float/double
  const char *FPtoFPFuncName = nullptr;
  switch (II.getIntrinsicID()) {
    // Variable Argument Handling Intrinsics
  case llvm::Intrinsic::vastart: {
    // va_list is just the last argument to the current function
    auto LegalListPointer = getMappedValue(II.getArgOperand(0));
    assert(LegalListPointer.isScalar() && "va_list operand must be scalar");
    auto *VAListPtr = LegalListPointer[0];
    auto *VarArgsPtr = NewFunc->getArg(NewFunc->arg_size() - 1);
    Builder.CreateStore(VarArgsPtr, VAListPtr);
    return;
  }
  case llvm::Intrinsic::vaend: {
    // No-op
    return;
  }
  case llvm::Intrinsic::vacopy: {
    auto DestListPtr = getMappedValue(II.getArgOperand(0));
    auto SrcListPtr = getMappedValue(II.getArgOperand(1));
    assert(DestListPtr.isScalar() && "va_copy dest operand must be scalar");
    assert(SrcListPtr.isScalar() && "va_copy src operand must be scalar");

    auto *CurrentState = Builder.CreateLoad(PtrTy, SrcListPtr[0]);
    Builder.CreateStore(CurrentState, DestListPtr[0]);
    return;
  }
    // C/C++ Library intrinsics
  case llvm::Intrinsic::memcpy:
  case llvm::Intrinsic::memmove:
  case llvm::Intrinsic::memset: {
    auto *Len = getMappedValue(II.getArgOperand(2))[0];
    if (Len->getType()->isIntegerTy(64)) {
      // Replace p0.i64 intrinsic with the p0.i32 version
      if (IntPtrTy->getIntegerBitWidth() == 32) {
        auto ID = II.getIntrinsicID();
        Len = Builder.CreateTrunc(Len, Int32Ty);
        llvm::SmallVector<llvm::Value *, 4> NewArgs;
        NewArgs.push_back(getMappedValue(II.getArgOperand(0))[0]);
        // The argument has to be an i8 to fulfill the intrinsic signature
        if (ID == llvm::Intrinsic::memset) {
          auto *TruncInst = llvm::CastInst::Create(
              llvm::Instruction::Trunc, getMappedValue(II.getArgOperand(1))[0],
              Int8Ty);
          Builder.Insert(TruncInst);
          NewArgs.push_back(TruncInst);
        } else {
          NewArgs.push_back(getMappedValue(II.getArgOperand(1))[0]);
        }
        NewArgs.push_back(Len);
        // volatile (always constant)
        NewArgs.push_back(II.getArgOperand(3));

        // Potentially declare new intrinsic
        llvm::SmallVector<llvm::Type *, 3> Tys;
        Tys.push_back(NewArgs[0]->getType());
        if (ID != llvm::Intrinsic::memset) {
          // for memcpy/memmove, src is overloaded (e.g. we need explicit p0)
          Tys.push_back(NewArgs[1]->getType());
        }
        Tys.push_back(Len->getType());

        llvm::Function *NewIntrinsic = llvm::Intrinsic::getOrInsertDeclaration(
            NewFunc->getParent(), ID, Tys);

        ValueMap[&II] = Builder.CreateCall(NewIntrinsic, NewArgs);
        return;
      }
      WATEVER_UNIMPLEMENTED("64-bit length in memory intrinsic");
    }
    break;
  }
    // clang-format off
  case llvm::Intrinsic::sin: FPtoFPFuncName = "sin"; break;
  case llvm::Intrinsic::cos: FPtoFPFuncName = "cos"; break;
  case llvm::Intrinsic::tan: FPtoFPFuncName = "tan"; break;
  case llvm::Intrinsic::asin: FPtoFPFuncName = "asin"; break;
  case llvm::Intrinsic::acos: FPtoFPFuncName = "acos"; break;
  case llvm::Intrinsic::atan: FPtoFPFuncName = "atan"; break;
  case llvm::Intrinsic::sinh: FPtoFPFuncName = "sinh"; break;
  case llvm::Intrinsic::cosh: FPtoFPFuncName = "cosh"; break;
  case llvm::Intrinsic::tanh: FPtoFPFuncName = "tanh"; break;
  case llvm::Intrinsic::exp: FPtoFPFuncName = "exp"; break;
  case llvm::Intrinsic::exp2: FPtoFPFuncName = "exp2"; break;
  case llvm::Intrinsic::exp10: FPtoFPFuncName = "exp10"; break;
  case llvm::Intrinsic::log: FPtoFPFuncName = "log"; break;
  case llvm::Intrinsic::log10: FPtoFPFuncName = "log10"; break;
  case llvm::Intrinsic::log2: FPtoFPFuncName = "log2"; break;
  case llvm::Intrinsic::fabs: FPtoFPFuncName = "fabs"; break;
  // clang-format on
  // Specialized Arithmetic Intrinsics
  case llvm::Intrinsic::fmuladd: {
    auto FirstLegalArg = getMappedValue(II.getArgOperand(0));
    auto SecondLegalArg = getMappedValue(II.getArgOperand(1));
    auto ThirdLegalArg = getMappedValue(II.getArgOperand(2));
    if (!FirstLegalArg.isScalar() || !SecondLegalArg.isScalar() ||
        !ThirdLegalArg.isScalar()) {
      WATEVER_UNIMPLEMENTED("fmuladd only supported on scalar types");
    }

    auto *Ty = FirstLegalArg[0]->getType();
    if (!Ty->isDoubleTy() && !Ty->isFloatTy()) {
      WATEVER_UNIMPLEMENTED(
          "fmuladd intrinsic only supported for float/double");
    }
    auto *MulRes = Builder.CreateFMul(FirstLegalArg[0], SecondLegalArg[0]);
    ValueMap[&II] = Builder.CreateFAdd(MulRes, ThirdLegalArg[0]);
    return;
  }
  // General Intrinsics
  case llvm::Intrinsic::ptrmask: {
    auto LegalPointer = getMappedValue(II.getOperand(0));
    auto LegalMask = getMappedValue(II.getOperand(1));
    if (!LegalPointer.isScalar() || !LegalMask.isScalar()) {
      WATEVER_UNIMPLEMENTED("ptrmask only supported on scalar types");
    }
    auto *PointerAsInt = Builder.CreatePtrToInt(LegalPointer[0], IntPtrTy);
    auto *Masked = Builder.CreateAnd(PointerAsInt, LegalMask[0]);
    ValueMap[&II] = Builder.CreateIntToPtr(Masked, PtrTy);
    return;
  }
  case llvm::Intrinsic::threadlocal_address: {
    if (!Config.EnabledFeatures.annotations_enabled()) {
      ValueMap[&II] = II.getArgOperand(0);
    } else {
      WATEVER_UNIMPLEMENTED("atomics support");
    }
    return;
  }
  default:
    break;
  }

  if (FPtoFPFuncName) {
    auto LegalArg = getMappedValue(II.getArgOperand(0));
    assert(LegalArg.isScalar() && "Math intrinsics only support scalars");

    auto *Ty = LegalArg[0]->getType();
    std::string LibCallName = FPtoFPFuncName;
    if (Ty->isFloatTy()) {
      LibCallName += "f";
    } else if (!Ty->isDoubleTy()) {
      WATEVER_UNIMPLEMENTED("Math intrinsic {} only supported for float/double",
                            FPtoFPFuncName);
    }
    ValueMap[&II] = emitLibCall(LibCallName, {LegalArg[0]}, Ty);
    return;
  }

  // If the backend can deal with an intrinsic - keep it as a call
  llvm::SmallVector<llvm::Value *> NewArgs;
  unsigned ArgIdx = 0;
  for (auto &Arg : II.args()) {
    auto LegalArg = getMappedValue(Arg.get());
    if (!LegalArg.isScalar()) {
      WATEVER_UNIMPLEMENTED("reconstruct the original llvm::Value, based on "
                            "the legalized argument");
    }
    auto *ValueArg = LegalArg[0];

    bool IsImmArg = II.paramHasAttr(ArgIdx, llvm::Attribute::ImmArg);
    ArgIdx++;

    // Value does not need preprocessing.
    if (ValueArg->getType() == Arg->getType()) {
      NewArgs.push_back(ValueArg);
      continue;
    }
    // If the argument is an immediate, we cannot preprocess it with a
    // Truncation, so the backend will need to be able to handle illegal types
    if (IsImmArg) {
      NewArgs.push_back(Arg);
      continue;
    }

    if (ValueArg->getType()->isIntegerTy()) {
      // This forces LLVM to emit the instruction - otherwise the builder
      // might do constant folding on constants (e.g., would not emit a trunc
      // i32 0 to i8
      auto *TruncInst = llvm::CastInst::Create(llvm::Instruction::Trunc,
                                               ValueArg, Arg->getType());
      Builder.Insert(TruncInst);
      NewArgs.push_back(TruncInst);
      continue;
    }

    WATEVER_UNIMPLEMENTED("unsupported delegalization of {}",
                          llvmToString(*ValueArg));
  }
  ValueMap[&II] =
      LegalValue{Builder.CreateCall(II.getCalledFunction(), NewArgs)};
  return;
}

llvm::Function *LegalizationPass::createLegalFunction(llvm::Module &Mod,
                                                      llvm::Function *OldFunc) {
  llvm::Function *Fn =
      llvm::Function::Create(getLegalFunctionType(OldFunc->getFunctionType()),
                             OldFunc->getLinkage(), OldFunc->getName(), Mod);

  // Copy all attributes of the function
  // TODO maybe handle parametere attributes too - however, they cannot just
  // be copied, as legalization potentially changes the signature
  llvm::AttributeSet FnAttrs = OldFunc->getAttributes().getFnAttrs();
  Fn->setAttributes(llvm::AttributeList::get(
      Fn->getContext(), llvm::AttributeList::FunctionIndex, FnAttrs));
  Fn->setVisibility(OldFunc->getVisibility());

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

  if (auto *ATy = llvm::dyn_cast<llvm::ArrayType>(Ty)) {
    llvm::SmallVector<llvm::Type *> Ts;
    auto LegalElementType = getLegalType(ATy->getElementType());
    for (unsigned I = 0; I < ATy->getNumElements(); ++I) {
      for (auto *ElementTyPart : LegalElementType) {
        Ts.push_back(ElementTyPart);
      }
    }
    return LegalType{Ts};
  }

  if (auto *STy = llvm::dyn_cast<llvm::StructType>(Ty)) {
    llvm::SmallVector<llvm::Type *> Ts;
    for (auto *ElementTy : STy->elements()) {
      auto LegalElementType = getLegalType(ElementTy);
      for (auto *ElementTyPart : LegalElementType) {
        Ts.push_back(ElementTyPart);
      }
    }
    return LegalType{Ts};
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
