#include <watever/graph.h>
#include <watever/utils.h>

using namespace watever;

inline ValueType Value::getValueType() const {
  return DefinedBy->getValueType(ResNo);
}

Value Graph::getNode(Opcode Op, ValueType RT, Value Arg1) {
  WATEVER_TODO("handle one arg nodes");
}
Value Graph::getNode(Opcode Op, ValueType RT, Value Arg1, Value Arg2) {
  WATEVER_TODO("handle two arg nodes");
  switch (Op) {
  case Opcode::ADD: {
    assert((RT == ValueType::I32 || RT == ValueType::I64) &&
           "This operator only applies to Integer types!");
    assert(Arg1.getValueType() == Arg2.getValueType() &&
           "Binary operator types must match!");
    break;
  }
  default:
    WATEVER_UNREACHABLE("Illegal signature for Opcode {}", Op);
  }
}

Value Graph::getNode(Opcode Op, ValueType RT, Value Arg1, Value Arg2,
                     Value Arg3) {
  WATEVER_TODO("handle one three nodes");
}

Value Graph::getNode(Opcode Op, std::vector<ValueType> RTs, Value Arg1) {
  WATEVER_TODO("handle one arg nodes");
}

// See SelectionDAG.cpp:7645
Value Graph::getNode(Opcode Op, std::vector<ValueType> RTs, Value Arg1,
                     Value Arg2) {
  WATEVER_TODO("handle two arg nodes");
}

Value Graph::getNode(Opcode Op, std::vector<ValueType> RTs, Value Arg1,
                     Value Arg2, Value Arg3) {
  WATEVER_TODO("handle three arg nodes");
}

void GraphBuilder::visit(const llvm::Instruction &I) {
  WATEVER_LOG_DBG("visiting {} with Opcode {}", llvmToString(I), I.getOpcode());
  if (I.isTerminator()) {
    WATEVER_TODO("handle PHI in successor");
  }

  visit(I.getOpcode(), I);
}

void GraphBuilder::visit(unsigned Opcode, const llvm::User &I) {
  switch (Opcode) {
  default:
    WATEVER_UNREACHABLE("Unknown instruction type encountered!");
    // TODO PtrToAddr is currently ignored (as not an Instruction)
#define HANDLE_INST(NUM, OPCODE, CLASS)                                        \
  case llvm::Instruction::OPCODE:                                              \
    visit##OPCODE((const llvm::CLASS &)I);                                     \
    break;
#include <watever/Instruction.def>
  }
}

void GraphBuilder::visitRet(const llvm::ReturnInst &I) {
  WATEVER_TODO("visitRet");
};
void GraphBuilder::visitBr(const llvm::BranchInst &I) {
  WATEVER_TODO("visitBr");
};
void GraphBuilder::visitSwitch(const llvm::SwitchInst &I) {
  WATEVER_TODO("visitSwitch");
};
void GraphBuilder::visitIndirectBr(const llvm::IndirectBrInst &I) {
  WATEVER_TODO("visitIndirectBr");
};
void GraphBuilder::visitUnreachable(const llvm::UnreachableInst &I) {
  WATEVER_TODO("visitUnreachable");
};
void GraphBuilder::visitCleanupRet(const llvm::CleanupReturnInst &I) {
  WATEVER_TODO("visitCleanupRet");
};
void GraphBuilder::visitCatchSwitch(const llvm::CatchSwitchInst &I) {
  WATEVER_TODO("visitCatchSwitch");
};
void GraphBuilder::visitCatchRet(const llvm::CatchReturnInst &I) {
  WATEVER_TODO("visitCatchRet");
};
void GraphBuilder::visitCatchPad(const llvm::CatchPadInst &I) {
  WATEVER_TODO("visitCatchPad");
};
void GraphBuilder::visitCleanupPad(const llvm::CleanupPadInst &CPI) {
  WATEVER_TODO("visitCleanupPad");
};
void GraphBuilder::visitInvoke(const llvm::InvokeInst &I) {
  WATEVER_TODO("visitInvoke");
};
void GraphBuilder::visitCallBr(const llvm::CallBrInst &I) {
  WATEVER_TODO("visitCallBr");
};
void GraphBuilder::visitCallBrLandingPad(const llvm::CallInst &I) {
  WATEVER_TODO("visitCallBrLandingPad");
};
void GraphBuilder::visitResume(const llvm::ResumeInst &I) {
  WATEVER_TODO("visitResume");
};
void GraphBuilder::visitUnary(const llvm::User &I, Opcode Op) {
  WATEVER_TODO("visitUnary");
};

void GraphBuilder::visitBinary(const llvm::User &I, Opcode Op) {
  WATEVER_TODO("visitBinary");
};
void GraphBuilder::visitShift(const llvm::User &I, Opcode Op) {
  WATEVER_TODO("visitShift");
};
void GraphBuilder::visitSDiv(const llvm::User &I) {
  WATEVER_TODO("visitSDiv");
};

void GraphBuilder::visitICmp(const llvm::ICmpInst &I) {
  WATEVER_TODO("visitICmp");
};
void GraphBuilder::visitFCmp(const llvm::FCmpInst &I) {
  WATEVER_TODO("visitFCmp");
};
void GraphBuilder::visitTrunc(const llvm::User &I) {
  WATEVER_TODO("visitTrunc");
};
void GraphBuilder::visitZExt(const llvm::User &I) {
  WATEVER_TODO("visitZExt");
};
void GraphBuilder::visitSExt(const llvm::User &I) {
  WATEVER_TODO("visitSExt");
};
void GraphBuilder::visitFPTrunc(const llvm::User &I) {
  WATEVER_TODO("visitFPTrunc");
};
void GraphBuilder::visitFPExt(const llvm::User &I) {
  WATEVER_TODO("visitFPExt");
};
void GraphBuilder::visitFPToUI(const llvm::User &I) {
  WATEVER_TODO("visitFPToUI");
};
void GraphBuilder::visitFPToSI(const llvm::User &I) {
  WATEVER_TODO("visitFPToSI");
};
void GraphBuilder::visitUIToFP(const llvm::User &I) {
  WATEVER_TODO("visitUIToFP");
};
void GraphBuilder::visitSIToFP(const llvm::User &I) {
  WATEVER_TODO("visitSIToFP");
};
void GraphBuilder::visitPtrToAddr(const llvm::User &I) {
  WATEVER_TODO("visitPtrToAddr");
};
void GraphBuilder::visitPtrToInt(const llvm::User &I) {
  WATEVER_TODO("visitPtrToInt");
};
void GraphBuilder::visitIntToPtr(const llvm::User &I) {
  WATEVER_TODO("visitIntToPtr");
};
void GraphBuilder::visitBitCast(const llvm::User &I) {
  WATEVER_TODO("visitBitCast");
};
void GraphBuilder::visitAddrSpaceCast(const llvm::User &I) {
  WATEVER_TODO("visitAddrSpaceCast");
};

void GraphBuilder::visitExtractElement(const llvm::User &I) {
  WATEVER_TODO("visitExtractElement");
};
void GraphBuilder::visitInsertElement(const llvm::User &I) {
  WATEVER_TODO("visitInsertElement");
};
void GraphBuilder::visitShuffleVector(const llvm::User &I) {
  WATEVER_TODO("visitShuffleVector");
};

void GraphBuilder::visitExtractValue(const llvm::ExtractValueInst &I) {
  WATEVER_TODO("visitExtractValue");
};
void GraphBuilder::visitInsertValue(const llvm::InsertValueInst &I) {
  WATEVER_TODO("visitInsertValue");
};
void GraphBuilder::visitLandingPad(const llvm::LandingPadInst &LP) {
  WATEVER_TODO("visitLandingPad");
};

void GraphBuilder::visitGetElementPtr(const llvm::User &I) {
  WATEVER_TODO("visitGetElementPtr");
};
void GraphBuilder::visitSelect(const llvm::User &I) {
  WATEVER_TODO("visitSelect");
};

void GraphBuilder::visitAlloca(const llvm::AllocaInst &I) {
  WATEVER_TODO("visitAlloca");
};
void GraphBuilder::visitLoad(const llvm::LoadInst &I) {
  WATEVER_TODO("visitLoad");
};
void GraphBuilder::visitStore(const llvm::StoreInst &I) {
  WATEVER_TODO("visitStore");
};
void GraphBuilder::visitAtomicCmpXchg(const llvm::AtomicCmpXchgInst &I) {
  WATEVER_TODO("visitAtomicCmpXchg");
};
void GraphBuilder::visitAtomicRMW(const llvm::AtomicRMWInst &I) {
  WATEVER_TODO("visitAtomicRMW");
};
void GraphBuilder::visitFence(const llvm::FenceInst &I) {
  WATEVER_TODO("visitFence");
};
void GraphBuilder::visitPHI(const llvm::PHINode &I) {
  WATEVER_TODO("visitPHI");
};
void GraphBuilder::visitCall(const llvm::CallInst &I) {
  WATEVER_TODO("visitCall");
};
void GraphBuilder::visitFreeze(const llvm::FreezeInst &I) {
  WATEVER_TODO("visitFreeze");
};

void GraphBuilder::visitVAArg(const llvm::VAArgInst &I) {
  WATEVER_TODO("visitVAArg");
};

void GraphBuilder::clear() { WATEVER_TODO("clear builder"); }
