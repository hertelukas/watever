#include <watever/legalization.h>

using namespace watever;

llvm::PreservedAnalyses
LegalizationPass::run(llvm::Function &F, llvm::FunctionAnalysisManager &AM) {
  WATEVER_LOG_DBG("Legalizing {}", F.getName().str());
  this->visit(F);

  WATEVER_LOG_DBG("Legalized Function:\n {}", llvmToString(F));
  // TODO this is not correct
  return llvm::PreservedAnalyses::all();
}

void LegalizationPass::visitAllocaInst(llvm::AllocaInst &AI) {
  // alloca doesn't need any legalization
}

void LegalizationPass::visitBinaryOperator(llvm::BinaryOperator &BO) {
  WATEVER_LOG_TRACE("Handling binary operator: {}", llvmToString(BO));
  bool Handled = false;
  switch (BO.getOpcode()) {
  case llvm::Instruction::Add: {
    Handled =
        legalizeIntegerBinaryOp(BO, [](auto &B, auto *LHS, auto *RHS, auto _) {
          return B.CreateAdd(LHS, RHS);
        });
    break;
  }
  case llvm::Instruction::FAdd: {
    break;
  }
  case llvm::Instruction::Sub: {
    Handled =
        legalizeIntegerBinaryOp(BO, [](auto &B, auto *LHS, auto *RHS, auto _) {
          return B.CreateSub(LHS, RHS);
        });
    break;
  }
  case llvm::Instruction::FSub: {
    break;
  }
  case llvm::Instruction::Mul: {
    Handled =
        legalizeIntegerBinaryOp(BO, [](auto &B, auto *LHS, auto *RHS, auto _) {
          return B.CreateMul(LHS, RHS);
        });
    break;
  }
  case llvm::Instruction::FMul:
  case llvm::Instruction::UDiv: {
    Handled = legalizeIntegerBinaryOp(
        BO, [](auto &B, auto *LHS, auto *RHS, auto Width) {
          // We might have trash in the upper bits of the value (as we will
          // don't really "zext"). This doesn't matter for values where higher
          // bits are ignored anyway (add, mul, sub, ...), or if we explicitly
          // Sext
          unsigned TargetBitWidth = LHS->getType()->getIntegerBitWidth();
          llvm::APInt Mask = llvm::APInt::getLowBitsSet(TargetBitWidth, Width);
          llvm::Value *MaskVal = llvm::ConstantInt::get(LHS->getType(), Mask);
          LHS = B.CreateAnd(LHS, MaskVal);
          RHS = B.CreateAnd(RHS, MaskVal);
          return B.CreateUDiv(LHS, RHS);
        });
    break;
  }
  case llvm::Instruction::SDiv: {
    auto *InstType = BO.getType();

    if (!InstType->isIntegerTy()) {
      WATEVER_TODO("expanding vector {} not supported", BO.getOpcodeName());
      break;
    }

    unsigned Width = InstType->getIntegerBitWidth();

    if (Width == 32 || Width == 64) {
      Handled = true;
      break;
    }

    if (Width >= 64) {
      WATEVER_TODO("expanding {} not supported", BO.getOpcodeName());
      break;
    }

    auto *TargetTy = Width < 32 ? Int32Ty : Int64Ty;
    llvm::IRBuilder<> Builder(&BO);
    // We have to do actual sign extension when lowering, cannot optimized away
    // but just "ignoring" upper bits
    // TODO LLVM seems to be doing that already
    llvm::Value *ExtA = Builder.CreateSExt(BO.getOperand(0), TargetTy);
    llvm::Value *ExtB = Builder.CreateSExt(BO.getOperand(1), TargetTy);
    llvm::Value *LegalDiv = Builder.CreateSDiv(ExtA, ExtB);
    llvm::Value *TruncResult = Builder.CreateTrunc(LegalDiv, BO.getType());
    BO.replaceAllUsesWith(TruncResult);
    BO.eraseFromParent();
    Handled = true;
    break;
  }
  case llvm::Instruction::FDiv:
    break;
  case llvm::Instruction::URem: {
    Handled = legalizeIntegerBinaryOp(
        BO, [](auto &B, auto *LHS, auto *RHS, auto Width) {
          unsigned TargetBitWidth = LHS->getType()->getIntegerBitWidth();
          llvm::APInt Mask = llvm::APInt::getLowBitsSet(TargetBitWidth, Width);
          llvm::Value *MaskVal = llvm::ConstantInt::get(LHS->getType(), Mask);
          LHS = B.CreateAnd(LHS, MaskVal);
          RHS = B.CreateAnd(RHS, MaskVal);
          return B.CreateURem(LHS, RHS);
        });
    break;
  }
  case llvm::Instruction::SRem: {
    auto *InstType = BO.getType();

    if (!InstType->isIntegerTy()) {
      WATEVER_TODO("expanding vector {} not supported", BO.getOpcodeName());
      break;
    }

    unsigned Width = InstType->getIntegerBitWidth();

    if (Width == 32 || Width == 64) {
      Handled = true;
      break;
    }

    if (Width >= 64) {
      WATEVER_TODO("expanding {} not supported", BO.getOpcodeName());
      break;
    }

    auto *TargetTy = Width < 32 ? Int32Ty : Int64Ty;
    llvm::IRBuilder<> Builder(&BO);
    // TODO LLVM seems to be doing that already
    llvm::Value *ExtA = Builder.CreateSExt(BO.getOperand(0), TargetTy);
    llvm::Value *ExtB = Builder.CreateSExt(BO.getOperand(1), TargetTy);
    llvm::Value *LegalRem = Builder.CreateSRem(ExtA, ExtB);
    llvm::Value *TruncResult = Builder.CreateTrunc(LegalRem, BO.getType());
    BO.replaceAllUsesWith(TruncResult);
    BO.eraseFromParent();
    Handled = true;
    break;
  }
  case llvm::Instruction::FRem:
    break;
  case llvm::Instruction::Shl: {
    Handled = legalizeIntegerBinaryOp(
        BO, [](auto &B, auto *LHS, auto *RHS, auto Width) {
          // TODO LLVM seems to be always doing some "legalization" already
          unsigned TargetBitWidth = LHS->getType()->getIntegerBitWidth();
          llvm::APInt Mask = llvm::APInt::getLowBitsSet(TargetBitWidth, Width);
          llvm::Value *MaskVal = llvm::ConstantInt::get(LHS->getType(), Mask);
          llvm::Value *LegalRHS = B.CreateAnd(RHS);
          return B.CreateShl(LHS, LegalRHS);
        });
    break;
  }
  case llvm::Instruction::LShr: {
    Handled = legalizeIntegerBinaryOp(
        BO, [](auto &B, auto *LHS, auto *RHS, auto Width) {
          unsigned TargetBitWidth = LHS->getType()->getIntegerBitWidth();
          llvm::APInt Mask = llvm::APInt::getLowBitsSet(TargetBitWidth, Width);
          llvm::Value *MaskVal = llvm::ConstantInt::get(LHS->getType(), Mask);
          LHS = B.CreateAnd(LHS, MaskVal);
          RHS = B.CreateAnd(RHS, MaskVal);
          return B.CreateLShr(LHS, RHS);
        });
    break;
  }
  case llvm::Instruction::AShr: {
    auto *InstType = BO.getType();

    if (!InstType->isIntegerTy()) {
      WATEVER_TODO("expanding vector {} not supported", BO.getOpcodeName());
      break;
    }

    unsigned Width = InstType->getIntegerBitWidth();

    if (Width == 32 || Width == 64) {
      Handled = true;
      break;
    }

    if (Width >= 64) {
      WATEVER_TODO("expanding {} not supported", BO.getOpcodeName());
      break;
    }
    // We have to clear the upper bits of the second argument, so we don't shift
    // too much, and sign extend the argument, so we shift in ones if negative
    auto *TargetTy = Width < 32 ? Int32Ty : Int64Ty;
    llvm::IRBuilder<> Builder(&BO);
    llvm::Value *ExtA = Builder.CreateSExt(BO.getOperand(0), TargetTy);

    llvm::Value *ExtB = Builder.CreateZExt(BO.getOperand(1), TargetTy);
    llvm::APInt Mask =
        llvm::APInt::getLowBitsSet(TargetTy->getIntegerBitWidth(), Width);
    llvm::Value *MaskVal = llvm::ConstantInt::get(ExtB->getType(), Mask);
    ExtB = Builder.CreateAnd(ExtB, MaskVal);

    llvm::Value *LegalAShr = Builder.CreateAShr(ExtA, ExtB);
    llvm::Value *TruncResult = Builder.CreateTrunc(LegalAShr, BO.getType());
    BO.replaceAllUsesWith(TruncResult);
    BO.eraseFromParent();
    Handled = true;
    break;
  }
  case llvm::Instruction::And: {
    Handled =
        legalizeIntegerBinaryOp(BO, [](auto &B, auto *LHS, auto *RHS, auto _) {
          return B.CreateAnd(LHS, RHS);
        });
    break;
  }
  case llvm::Instruction::Or: {
    Handled =
        legalizeIntegerBinaryOp(BO, [](auto &B, auto *LHS, auto *RHS, auto _) {
          return B.CreateOr(LHS, RHS);
        });
    break;
  }
  case llvm::Instruction::Xor: {
    Handled =
        legalizeIntegerBinaryOp(BO, [](auto &B, auto *LHS, auto *RHS, auto _) {
          return B.CreateXor(LHS, RHS);
        });
    break;
  }
  default:
    WATEVER_UNREACHABLE("Illegal opcode encountered: {}", BO.getOpcodeName());
    break;
  }

  if (!Handled)
    WATEVER_LOG_WARN("Opcode {} has not been handled", BO.getOpcodeName());
}

// TODO handle vectorized GEP
void LegalizationPass::visitGetElementPtrInst(llvm::GetElementPtrInst &GI) {
  const llvm::DataLayout &DL = GI.getModule()->getDataLayout();

  llvm::APInt Offset(DL.getPointerSizeInBits(GI.getPointerAddressSpace()), 0);
  if (GI.accumulateConstantOffset(DL, Offset)) {
    WATEVER_LOG_TRACE("GEP with constant offset of {}", Offset.getSExtValue());
    if (Offset.isZero()) {
      // Just send the ptr itself
      GI.replaceAllUsesWith(GI.getOperand(0));
    } else {
      llvm::Value *OffsetVal = llvm::ConstantInt::get(IntPtrTy, Offset);
      llvm::IRBuilder<> Builder(&GI);
      llvm::Value *PtrAsInt =
          Builder.CreatePtrToInt(GI.getOperand(0), IntPtrTy);
      llvm::Value *PtrWithOffsetAsInt = Builder.CreateAdd(PtrAsInt, OffsetVal);
      llvm::Value *PtrWithOffset =
          Builder.CreateIntToPtr(PtrWithOffsetAsInt, PtrTy);
      GI.replaceAllUsesWith(PtrWithOffset);
    }
    GI.eraseFromParent();
    return;
  }

  llvm::IRBuilder<> Builder(&GI);
  llvm::Type *CurrentTy = GI.getSourceElementType();
  llvm::Value *TotalOffset = llvm::ConstantInt::get(IntPtrTy, 0);

  auto *IdxIt = GI.idx_begin();

  llvm::Value *FirstIndex = IdxIt->get();
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
    llvm::Value *Index = IdxIt->get();
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

  llvm::Value *BasePtr = GI.getPointerOperand();
  llvm::Value *BasePtrInt = Builder.CreatePtrToInt(BasePtr, IntPtrTy);
  llvm::Value *NewPtrInt = Builder.CreateAdd(BasePtrInt, TotalOffset);
  llvm::Value *NewPtr = Builder.CreateIntToPtr(NewPtrInt, PtrTy);

  GI.replaceAllUsesWith(NewPtr);
  GI.eraseFromParent();
}

void LegalizationPass::visitRet(llvm::ReturnInst &RI) {
  WATEVER_TODO("handle return instruction");
}

void LegalizationPass::visitSExtInst(llvm::SExtInst &SI) {
  auto *InstType = SI.getType();

  if (!InstType->isIntegerTy()) {
    WATEVER_TODO("expanding vector {} not supported", SI.getOpcodeName());
    return;
  }

  unsigned ToWidth = InstType->getIntegerBitWidth();

  // TODO we probably also cannot legalize non i8, i16, i32, i64 from widths
  if (ToWidth == 32 || ToWidth == 64) {
    return;
  }

  unsigned FromWidth = SI.getOperand(0)->getType()->getIntegerBitWidth();

  if (ToWidth >= 64) {
    WATEVER_TODO("expanding {} not supported", SI.getOpcodeName());
    return;
  }

  // If the target width is not i32 or i64, we can simulate an aribtrary signed
  // extension with a shift left, followed by an arithmetic shift.

  auto *TargetTy = ToWidth < 32 ? Int32Ty : Int64Ty;
  unsigned TargetWidth = ToWidth < 32 ? 32 : 64;

  llvm::IRBuilder<> Builder(&SI);
  llvm::Value *WideOperand = Builder.CreateZExt(SI.getOperand(0), TargetTy);
  llvm::Value *MaskVal =
      llvm::ConstantInt::get(TargetTy, TargetWidth - FromWidth);
  llvm::Value *Shl = Builder.CreateShl(WideOperand, MaskVal);
  llvm::Value *Shr = Builder.CreateAShr(Shl, MaskVal);
  llvm::Value *TruncResult = Builder.CreateTrunc(Shr, InstType);
  SI.replaceAllUsesWith(TruncResult);
  SI.eraseFromParent();
}

// I think we can pollute upper bits always, so we should just be able to ignore
// truncation. A solution to this would be to handle in a first pass only
// Truncs, and in a second ignore them
void LegalizationPass::visitTruncInst(llvm::TruncInst &TI) {
  WATEVER_TODO("decide wether truncating should AND the result");
}

void LegalizationPass::visitZExtInst(llvm::ZExtInst &ZI) {
  auto *InstType = ZI.getType();

  if (!InstType->isIntegerTy()) {
    WATEVER_TODO("expanding vector {} not supported", ZI.getOpcodeName());
    return;
  }

  unsigned Width = InstType->getIntegerBitWidth();

  if (Width == 32 || Width == 64) {
    return;
  }

  // TODO decide wether truncating should AND the result. In theory it should
  // but this leads to quite some inefficencies where we zext to legalize,
  // don't care about upper bits. (e.g., add)
  WATEVER_TODO("zext is not really legal yet");
}
