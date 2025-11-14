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
    Handled = legalizeIntegerBinaryOp(BO, [](auto &B, auto *LHS, auto *RHS) {
      return B.CreateAdd(LHS, RHS);
    });
    break;
  }
  case llvm::Instruction::FAdd: {
    break;
  }
  case llvm::Instruction::Sub: {
    Handled = legalizeIntegerBinaryOp(BO, [](auto &B, auto *LHS, auto *RHS) {
      return B.CreateSub(LHS, RHS);
    });
    break;
  }
  case llvm::Instruction::FSub: {
    break;
  }
  case llvm::Instruction::Mul: {
    Handled = legalizeIntegerBinaryOp(BO, [](auto &B, auto *LHS, auto *RHS) {
      return B.CreateMul(LHS, RHS);
    });
    break;
  }
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
    break;
  case llvm::Instruction::And: {
    Handled = legalizeIntegerBinaryOp(BO, [](auto &B, auto *LHS, auto *RHS) {
      return B.CreateAnd(LHS, RHS);
    });
    break;
  }
  case llvm::Instruction::Or: {
    Handled = legalizeIntegerBinaryOp(
        BO, [](auto &B, auto *LHS, auto *RHS) { return B.CreateOr(LHS, RHS); });
    break;
  }
  case llvm::Instruction::Xor: {
    Handled = legalizeIntegerBinaryOp(BO, [](auto &B, auto *LHS, auto *RHS) {
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
