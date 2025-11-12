#ifndef GRAPH_BUILDER_H
#define GRAPH_BUILDER_H

#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <watever/graph.h>

namespace watever {
class GraphBuilder {
  const llvm::Instruction *CurInst = nullptr;

  /// getValue - Return a Value for the given LLVM Value, but.
  Value getValue(const llvm::Value *V);

  // The Opcode is an LLVM Opcode
  void visit(unsigned Opcode, const llvm::User &I);

  // Terminator
  void visitRet(const llvm::ReturnInst &I);
  void visitBr(const llvm::BranchInst &I);
  void visitSwitch(const llvm::SwitchInst &I);
  void visitIndirectBr(const llvm::IndirectBrInst &I);
  void visitUnreachable(const llvm::UnreachableInst &I);
  void visitCleanupRet(const llvm::CleanupReturnInst &I);
  void visitCatchSwitch(const llvm::CatchSwitchInst &I);
  void visitCatchRet(const llvm::CatchReturnInst &I);
  void visitCatchPad(const llvm::CatchPadInst &I);
  void visitCleanupPad(const llvm::CleanupPadInst &CPI);

  // TODO these are handled separately in LLVM
  void visitInvoke(const llvm::InvokeInst &I);
  void visitCallBr(const llvm::CallBrInst &I);
  void visitCallBrLandingPad(const llvm::CallInst &I);
  void visitResume(const llvm::ResumeInst &I);

  void visitUnary(const llvm::User &I, Opcode Op);
  void visitFNeg(const llvm::User &I) { visitUnary(I, Opcode::FNEG); }

  void visitBinary(const llvm::User &I, Opcode Op);
  void visitShift(const llvm::User &I, Opcode Op);
  void visitAdd(const llvm::User &I) { visitBinary(I, Opcode::ADD); }
  void visitFAdd(const llvm::User &I) { visitBinary(I, Opcode::FADD); }
  void visitSub(const llvm::User &I) { visitBinary(I, Opcode::SUB); }
  void visitFSub(const llvm::User &I) { visitBinary(I, Opcode::FSUB); }
  void visitMul(const llvm::User &I) { visitBinary(I, Opcode::MUL); }
  void visitFMul(const llvm::User &I) { visitBinary(I, Opcode::FMUL); }
  void visitURem(const llvm::User &I) { visitBinary(I, Opcode::UREM); }
  void visitSRem(const llvm::User &I) { visitBinary(I, Opcode::SREM); }
  void visitFRem(const llvm::User &I) { visitBinary(I, Opcode::FREM); }
  void visitUDiv(const llvm::User &I) { visitBinary(I, Opcode::UDIV); }
  void visitSDiv(const llvm::User &I);
  void visitFDiv(const llvm::User &I) { visitBinary(I, Opcode::FDIV); }
  void visitAnd(const llvm::User &I) { visitBinary(I, Opcode::AND); }
  void visitOr(const llvm::User &I) { visitBinary(I, Opcode::OR); }
  void visitXor(const llvm::User &I) { visitBinary(I, Opcode::XOR); }
  void visitShl(const llvm::User &I) { visitShift(I, Opcode::SHL); }
  void visitLShr(const llvm::User &I) { visitShift(I, Opcode::SRL); }
  void visitAShr(const llvm::User &I) { visitShift(I, Opcode::SRA); }
  void visitICmp(const llvm::ICmpInst &I);
  void visitFCmp(const llvm::FCmpInst &I);
  // Visit the conversion instructions
  void visitTrunc(const llvm::User &I);
  void visitZExt(const llvm::User &I);
  void visitSExt(const llvm::User &I);
  void visitFPTrunc(const llvm::User &I);
  void visitFPExt(const llvm::User &I);
  void visitFPToUI(const llvm::User &I);
  void visitFPToSI(const llvm::User &I);
  void visitUIToFP(const llvm::User &I);
  void visitSIToFP(const llvm::User &I);
  void visitPtrToAddr(const llvm::User &I);
  void visitPtrToInt(const llvm::User &I);
  void visitIntToPtr(const llvm::User &I);
  void visitBitCast(const llvm::User &I);
  void visitAddrSpaceCast(const llvm::User &I);

  void visitExtractElement(const llvm::User &I);
  void visitInsertElement(const llvm::User &I);
  void visitShuffleVector(const llvm::User &I);

  void visitExtractValue(const llvm::ExtractValueInst &I);
  void visitInsertValue(const llvm::InsertValueInst &I);
  void visitLandingPad(const llvm::LandingPadInst &LP);

  void visitGetElementPtr(const llvm::User &I);
  void visitSelect(const llvm::User &I);

  void visitAlloca(const llvm::AllocaInst &I);
  void visitLoad(const llvm::LoadInst &I);
  void visitStore(const llvm::StoreInst &I);
  // void visitMaskedLoad(const llvm::CallInst &I, bool IsExpanding = false);
  // void visitMaskedStore(const llvm::CallInst &I, bool IsCompressing =
  // false); void visitMaskedGather(const CallInst &I); void
  // visitMaskedScatter(const CallInst &I);
  void visitAtomicCmpXchg(const llvm::AtomicCmpXchgInst &I);
  void visitAtomicRMW(const llvm::AtomicRMWInst &I);
  void visitFence(const llvm::FenceInst &I);
  void visitPHI(const llvm::PHINode &I);
  void visitCall(const llvm::CallInst &I);
  // bool visitMemCmpBCmpCall(const CallInst &I);
  // bool visitMemPCpyCall(const CallInst &I);
  // bool visitMemChrCall(const CallInst &I);
  // bool visitStrCpyCall(const CallInst &I, bool isStpcpy);
  // bool visitStrCmpCall(const CallInst &I);
  // bool visitStrLenCall(const CallInst &I);
  // bool visitStrNLenCall(const CallInst &I);
  // bool visitUnaryFloatCall(const CallInst &I, unsigned Opcode);
  // bool visitBinaryFloatCall(const CallInst &I, unsigned Opcode);
  // void visitAtomicLoad(const LoadInst &I);
  // void visitAtomicStore(const StoreInst &I);
  // void visitLoadFromSwiftError(const LoadInst &I);
  // void visitStoreToSwiftError(const StoreInst &I);
  void visitFreeze(const llvm::FreezeInst &I);

  void visitVAArg(const llvm::VAArgInst &I);

public:
  // TODO SelectionDAGBuilder:1330
  void visit(const llvm::Instruction &I);

  /// Clear out current Graph and the associated state and prepare this
  /// Builder for a new block.
  void clear();
};

} // namespace watever
#endif /* GRAPH_BUILDER_H */
