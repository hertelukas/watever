#ifndef GRAPH_H
#define GRAPH_H

#include <format>
#include <llvm/ADT/ilist.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Operator.h>
#include <llvm/IR/Value.h>
#include <vector>

namespace watever {

enum class ValueType { I32, I64, F32, F64, Other, Glue };

/// Opcodes which are legal (can be lowered) for Wasm
enum class Opcode {
  EntryToken,
  Glue,
  Other,
  FNEG,
  ADD,
  FADD,
  SUB,
  FSUB,
  MUL,
  FMUL,
  UREM,
  SREM,
  FREM,
  UDIV,
  FDIV,
  AND,
  OR,
  XOR,
  SHL,
  SRL,
  SRA
};

class Node;
class Use;
class Graph;

/// Output of a GraphNode
class Value {
  /// Node, which defines this value.
  Node *DefinedBy;
  /// The index of the return value of the node we are using.
  unsigned ResNo = 0;

public:
  /// Return the ValueType of the referenced return value.
  inline ValueType getValueType() const;
};

/// Represents an operation
class Node : public llvm::ilist_node<Node> {
  Opcode Op;

  /// The values used by this operation.
  Use *const Operands;
  unsigned NumOperands;

  /// The types produced by this operation.
  // TODO probably not efficient to allocate such a short vector
  const std::vector<ValueType> ResultTypes;

  /// List to all users of the results of this node.
  Use *UseList;

public:
  Node(Opcode Op, const std::vector<ValueType> RTs, Use *Ops,
       unsigned NumOperands)
      : Op{Op}, ResultTypes{RTs}, Operands{Ops}, NumOperands{NumOperands} {}

  ValueType getValueType(unsigned ResNo) const { return ResultTypes.at(ResNo); }
};

/// Input to a Node.
class Use {
  Value Val;
  Node *const User;

  /// Linked list to all results of the node which created this value.
  Use **Prev = nullptr;
  Use *Next = nullptr;

public:
  Use(Value V, Node *U) : Val(V), User(U) {}
};

/// Represenets a single basic block.
class Graph {
  Node EntryNode;

  llvm::ilist<Node> AllNodes;

public:
  // TODO store entry node in all nodes?
  Graph()
      : EntryNode(Opcode::EntryToken,
                  std::vector<ValueType>{ValueType::Other, ValueType::Glue},
                  nullptr, 0) {}

  Value getNode(Opcode Op, ValueType RT, Value Arg1);
  Value getNode(Opcode Op, ValueType RT, Value Arg1, Value Arg2);
  Value getNode(Opcode Op, ValueType RT, Value Arg1, Value Arg2, Value Arg3);
  Value getNode(Opcode Op, std::vector<ValueType> RTs, Value Arg1);
  Value getNode(Opcode Op, std::vector<ValueType> RTs, Value Arg1, Value Arg2);
  Value getNode(Opcode Op, std::vector<ValueType> RTs, Value Arg1, Value Arg2,
                Value Arg3);
};

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
  // void visitMaskedStore(const llvm::CallInst &I, bool IsCompressing = false);
  // void visitMaskedGather(const CallInst &I);
  // void visitMaskedScatter(const CallInst &I);
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

  /// Clear out current Graph and the associated state and prepare this Builder
  /// for a new block.
  void clear();
};

} // namespace watever

template <>
struct std::formatter<watever::Opcode> : std::formatter<std::string_view> {
  auto format(watever::Opcode Op, auto &Ctx) const {
    std::string_view Name = "Unknown";
    switch (Op) {
    case watever::Opcode::EntryToken:
      Name = "EntryToken";
      break;
    case watever::Opcode::Glue:
      Name = "Glue";
      break;
    case watever::Opcode::Other:
      Name = "Other";
      break;
    case watever::Opcode::FNEG:
      Name = "FNEG";
      break;
    case watever::Opcode::ADD:
      Name = "ADD";
      break;
    case watever::Opcode::FADD:
      Name = "FADD";
      break;
    case watever::Opcode::SUB:
      Name = "SUB";
      break;
    case watever::Opcode::FSUB:
      Name = "FSUB";
      break;
    case watever::Opcode::MUL:
      Name = "MUL";
      break;
    case watever::Opcode::FMUL:
      Name = "FMUL";
      break;
    case watever::Opcode::UREM:
      Name = "UREM";
      break;
    case watever::Opcode::SREM:
      Name = "SREM";
      break;
    case watever::Opcode::FREM:
      Name = "FREM";
      break;
    case watever::Opcode::UDIV:
      Name = "UDIV";
      break;
    case watever::Opcode::FDIV:
      Name = "FDIV";
      break;
    case watever::Opcode::AND:
      Name = "AND";
      break;
    case watever::Opcode::OR:
      Name = "OR";
      break;
    case watever::Opcode::XOR:
      Name = "XOR";
      break;
    case watever::Opcode::SHL:
      Name = "SHL";
      break;
    case watever::Opcode::SRL:
      Name = "SRL";
      break;
    case watever::Opcode::SRA:
      Name = "SRA";
      break;
    }
    return std::formatter<std::string_view>::format(Name, Ctx);
  }
};

template <>
struct std::formatter<watever::ValueType> : std::formatter<std::string_view> {
  auto format(watever::ValueType VT, auto &Ctx) const {
    std::string_view Name = "Unknown";
    switch (VT) {
    case watever::ValueType::I32:
      Name = "i32";
      break;
    case watever::ValueType::I64:
      Name = "i64";
      break;
    case watever::ValueType::F32:
      Name = "f32";
      break;
    case watever::ValueType::F64:
      Name = "f64";
      break;
    case watever::ValueType::Other:
      Name = "Other";
      break;
    case watever::ValueType::Glue:
      Name = "Glue";
      break;
    }
    return std::formatter<std::string_view>::format(Name, Ctx);
  }
};

#endif /* GRAPH_H */
