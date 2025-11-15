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
  WATEVER_LOG_DBG("Handling alloca instruction: {}", llvmToString(AI));
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
    llvm::Value *LegalAdd = Builder.CreateSDiv(ExtA, ExtB);
    llvm::Value *TruncResult = Builder.CreateTrunc(LegalAdd, BO.getType());
    BO.replaceAllUsesWith(TruncResult);
    BO.eraseFromParent();
  }
  case llvm::Instruction::FDiv:
  case llvm::Instruction::URem:
  case llvm::Instruction::SRem:
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
  case llvm::Instruction::LShr:
  case llvm::Instruction::AShr:
    break;
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

void LegalizationPass::visitRet(llvm::ReturnInst &RI) {
  WATEVER_TODO("Handle return instruction");
}
