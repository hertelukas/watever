#include "watever/ir.hpp"
#include "watever/linking.hpp"
#include "watever/opcode.hpp"
#include "watever/printer.hpp"
#include "watever/symbol.hpp"
#include "watever/type.hpp"
#include "watever/utils.hpp"
#include <algorithm>
#include <cstdint>
#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/Analysis/LoopNestAnalysis.h>
#include <llvm/IR/Argument.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/Casting.h>
#include <memory>
#include <ranges>

using namespace watever;

void Module::flattenConstant(const llvm::Constant *C,
                             std::vector<uint8_t> &Buffer,
                             llvm::SmallVector<RelocationEntry> &Relocs) {
  // Simple data arrays
  if (auto *CDS = llvm::dyn_cast<llvm::ConstantDataSequential>(C)) {
    llvm::StringRef RawData = CDS->getRawDataValues();
    Buffer.insert(Buffer.end(), RawData.begin(), RawData.end());
    return;
  }

  // Aggregates
  if (auto *CA = llvm::dyn_cast<llvm::ConstantAggregate>(C)) {
    for (unsigned I = 0; I != CA->getNumOperands(); ++I) {
      flattenConstant(CA->getOperand(I), Buffer, Relocs);
    }
    return;
  }

  // Integer constants
  if (auto *CI = llvm::dyn_cast<llvm::ConstantInt>(C)) {
    unsigned Width = CI->getBitWidth();
    uint64_t Val = CI->getZExtValue();
    if (Width <= 8)
      appendBytes(Buffer, static_cast<uint8_t>(Val));
    else if (Width <= 16)
      appendBytes(Buffer, static_cast<uint16_t>(Val));
    else if (Width <= 32)
      appendBytes(Buffer, static_cast<uint32_t>(Val));
    else if (Width <= 64)
      appendBytes(Buffer, static_cast<uint64_t>(Val));
    else {
      WATEVER_UNIMPLEMENTED("handle constnat int >64 bit");
    }
    return;
  }

  // zero initializers
  if (llvm::isa<llvm::ConstantAggregateZero>(C)) {
    WATEVER_UNIMPLEMENTED("figure out alloc size");
    return;
  }

  if (auto *GV = llvm::dyn_cast<llvm::GlobalValue>(C)) {
    WATEVER_UNIMPLEMENTED("handle data relocation to {}", llvmToString(*GV));
  }

  if (auto *_ = llvm::dyn_cast<llvm::ConstantExpr>(C)) {
    WATEVER_UNIMPLEMENTED("handle constant expressions in data section");
  }

  WATEVER_UNIMPLEMENTED("constant data of type {} is not yet supported",
                        llvmToString(*C->getType()));
}

llvm::DenseMap<llvm::Value *, int>
BlockLowering::getDependencyTreeUserCount(llvm::Instruction *Root) const {
  llvm::DenseMap<llvm::Value *, int> Result;

  llvm::SmallVector<llvm::Value *> WorkList;
  llvm::SmallPtrSet<llvm::Value *, 16> ASTNodes;

  WorkList.push_back(Root);
  ASTNodes.insert(Root);

  size_t Idx = 0;
  // Store all nodes in the AST
  while (Idx < WorkList.size()) {
    auto *Val = WorkList[Idx++];
    if (auto *Inst = llvm::dyn_cast<llvm::Instruction>(Val)) {
      for (llvm::Value *Op : Inst->operands()) {
        if (auto *OpInst = llvm::dyn_cast<llvm::Instruction>(Op)) {
          if (ASTNodes.insert(OpInst).second) {
            WorkList.push_back(OpInst);
          }
        }
      }
    }
  }
  for (auto *Node : ASTNodes) {
    int Count = 0;
    for (auto *User : Node->users()) {
      if (ASTNodes.contains(User)) {
        Count++;
      }
    }
    Result[Node] = Count;
  }
  return Result;
}

bool BlockLowering::hasExternalUser(llvm::Value *Val) {
  for (auto *User : Val->users()) {
    if (auto *Inst = llvm::dyn_cast<llvm::Instruction>(User)) {
      if (Inst->getParent() != BB || llvm::isa<llvm::PHINode>(Inst)) {
        return true;
      }
    }
  }
  return false;
}

void BlockLowering::handleIntrinsic(llvm::CallInst &CI) {
  switch (CI.getCalledFunction()->getIntrinsicID()) {
  default: {
    WATEVER_TODO("handle {} intrinsic",
                 CI.getCalledFunction()->getName().str());
  }
  }
}

//===----------------------------------------------------------------------===//
// Unary Operations
//===----------------------------------------------------------------------===//
void BlockLowering::visitUnaryOperator(llvm::UnaryOperator &UO) {
  addOperandsToWorklist(UO.operands());

  switch (UO.getOpcode()) {
  case llvm::Instruction::FNeg: {
    if (UO.getType()->isDoubleTy()) {
      Actions.Insts.emplace_back(Opcode::F64Neg);
    } else if (UO.getType()->isFloatTy()) {
      Actions.Insts.emplace_back(Opcode::F32Neg);
    } else {
      WATEVER_UNREACHABLE("Illegal floating point for negation: {}",
                          llvmToString(*UO.getType()));
    }
    break;
  }
  default:
    WATEVER_UNREACHABLE("Illegal opcode encountered: {}", UO.getOpcodeName());
    break;
  }
}

//===----------------------------------------------------------------------===//
// Binary Operations
//===----------------------------------------------------------------------===//
void BlockLowering::visitBinaryOperator(llvm::BinaryOperator &BO) {
  const auto *Ty = BO.getType();
  // TODO handle vectors
  const unsigned Width = Ty->getPrimitiveSizeInBits();
  bool Handled = true;

  addOperandsToWorklist(BO.operands());

  auto Dispatch = [&](Opcode::Enum Op32, Opcode::Enum Op64) {
    if (Width == 32 || Width == 1)
      Actions.Insts.emplace_back(Op32);
    else if (Width == 64)
      Actions.Insts.emplace_back(Op64);
    else
      Handled = false;
  };

  switch (BO.getOpcode()) {
  case llvm::Instruction::Add: {
    Dispatch(Opcode::I32Add, Opcode::I64Add);
    break;
  }
  case llvm::Instruction::FAdd: {
    Dispatch(Opcode::F32Add, Opcode::F64Add);
    break;
  }
  case llvm::Instruction::Sub: {
    Dispatch(Opcode::I32Sub, Opcode::I64Sub);
    break;
  }
  case llvm::Instruction::FSub: {
    Dispatch(Opcode::F32Sub, Opcode::F64Sub);
    break;
  }
  case llvm::Instruction::Mul: {
    Dispatch(Opcode::I32Mul, Opcode::I64Mul);
    break;
  }
  case llvm::Instruction::FMul: {
    Dispatch(Opcode::F32Mul, Opcode::F64Mul);
    break;
  }
  case llvm::Instruction::UDiv: {
    Dispatch(Opcode::I32DivU, Opcode::I64DivU);
    break;
  }
  case llvm::Instruction::SDiv: {
    Dispatch(Opcode::I32DivS, Opcode::I64DivS);
    break;
  }
  case llvm::Instruction::FDiv: {
    Dispatch(Opcode::F32Div, Opcode::F64Div);
    break;
  }
  case llvm::Instruction::URem: {
    Dispatch(Opcode::I32RemU, Opcode::I64RemU);
    break;
  }
  case llvm::Instruction::SRem: {
    Dispatch(Opcode::I32RemS, Opcode::I64RemS);
    break;
  }
  case llvm::Instruction::FRem: {
    Handled = false;
    break;
  }
  case llvm::Instruction::Shl: {
    Dispatch(Opcode::I32Shl, Opcode::I64Shl);
    break;
  }
  case llvm::Instruction::LShr: {
    Dispatch(Opcode::I32ShrU, Opcode::I64ShrU);
    break;
  }
  case llvm::Instruction::AShr: {
    Dispatch(Opcode::I32ShrS, Opcode::I64ShrS);
    break;
  }
  case llvm::Instruction::And: {
    Dispatch(Opcode::I32And, Opcode::I64And);
    break;
  }
  case llvm::Instruction::Or: {
    Dispatch(Opcode::I32Or, Opcode::I64Or);
    break;
  }
  case llvm::Instruction::Xor: {
    Dispatch(Opcode::I32Xor, Opcode::I64Xor);
    break;
  }
  default:
    WATEVER_UNREACHABLE("Illegal opcode encountered: {}", BO.getOpcodeName());
    break;
  }
  if (!Handled) {
    WATEVER_TODO("lowering of {}-bit {}", Width, BO.getOpcodeName());
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

void BlockLowering::visitAllocaInst(llvm::AllocaInst &AI) {
  auto *AllocatedType = AI.getAllocatedType();
  const uint32_t Size =
      AI.getModule()->getDataLayout().getTypeAllocSize(AllocatedType);

  ImportedGlobal *StackPointer;
  auto PointerType =
      AI.getModule()->getDataLayout().getPointerSizeInBits() == 64
          ? ValType::I64
          : ValType::I32;
  StackPointer = M.getStackPointer(PointerType);

  if (Size == 0) {
    Actions.Insts.emplace_back(Opcode::GlobalGet, StackPointer);
    return;
  }

  // Note that the instructions are evaluated in reverse order

  // We need the modified stack pointer on top of the stack, there  is no
  // global tee
  Actions.Insts.emplace_back(Opcode::GlobalGet, StackPointer);
  Actions.Insts.emplace_back(Opcode::GlobalSet, StackPointer);

  // Subtract the stack pointer
  if (PointerType == ValType::I64) {
    Actions.Insts.emplace_back(Opcode::I64Sub);
    Actions.Insts.emplace_back(Opcode::I64Const, Size);
  } else {
    Actions.Insts.emplace_back(Opcode::I32Sub);
    Actions.Insts.emplace_back(Opcode::I32Const, Size);
  }
  Actions.Insts.emplace_back(Opcode::GlobalGet, StackPointer);
}

void BlockLowering::doGreedyMemOp(llvm::Instruction &I, Opcode::Enum Op) {
  llvm::Value *Ptr = llvm::getLoadStorePointerOperand(&I);
  if (!Ptr) {
    WATEVER_UNREACHABLE("could not get pointer from memory operation");
  }

  if (auto *IntToPtr = llvm::dyn_cast<llvm::IntToPtrInst>(Ptr)) {
    if (auto *BinOp =
            llvm::dyn_cast<llvm::BinaryOperator>(IntToPtr->getOperand(0))) {
      // If we are the only user, and it's an addition, we can inline
      if (BinOp->getNumUses() <= 1 &&
          BinOp->getOpcode() == llvm::Instruction::Add) {
        if (auto *Offset =
                llvm::dyn_cast<llvm::ConstantInt>(BinOp->getOperand(1))) {
          WATEVER_LOG_TRACE("inlining offset");
          Actions.Insts.emplace_back(
              Op, std::make_unique<MemArg>(0, Offset->getZExtValue()));
          // We need the pointer (without the offset) on top of the stack
          WorkList.push_back(BinOp->getOperand(0));
          return;
        }
      }
    }
  }
  Actions.Insts.emplace_back(Op, std::make_unique<MemArg>());
  WorkList.push_back(Ptr);
}

void BlockLowering::visitLoadInst(llvm::LoadInst &LI) {
  // Loads should happen in sext or zext, if they need to get extended.
  auto *LoadType = LI.getType();
  // uint32_t Alignment = LI.getAlign().value();

  // TODO use alignment
  if (LoadType->isIntegerTy()) {
    const unsigned Width = LoadType->getIntegerBitWidth();

    if (Width == 32) {
      doGreedyMemOp(LI, Opcode::I32Load);
      return;
    }
    if (Width == 64) {
      doGreedyMemOp(LI, Opcode::I64Load);
      return;
    }
    // i8 and i16 should be loaded over sext/zext
    WATEVER_UNREACHABLE("Cannot load integer of width {}", Width);
  }

  if (LoadType->isFloatingPointTy()) {
    if (LoadType->isDoubleTy()) {
      doGreedyMemOp(LI, Opcode::F64Load);
      return;
    }
    if (LoadType->isFloatTy()) {
      doGreedyMemOp(LI, Opcode::F32Load);
      return;
    }
    WATEVER_UNIMPLEMENTED("Unsupported floating point type {} for load",
                          llvmToString(*LoadType));
  }

  if (LoadType->isPointerTy()) {
    if (LI.getModule()->getDataLayout().getPointerSizeInBits() == 64) {
      doGreedyMemOp(LI, Opcode::I64Load);
    } else {
      doGreedyMemOp(LI, Opcode::I32Load);
    }
    return;
  }

  WATEVER_UNREACHABLE("Unsupported load type {}", llvmToString(*LoadType));
}

void BlockLowering::visitStoreInst(llvm::StoreInst &SI) {
  auto *StoreType = SI.getOperand(0)->getType();

  if (StoreType->isIntegerTy()) {
    if (auto *TruncInst = llvm::dyn_cast<llvm::TruncInst>(SI.getOperand(0))) {
      auto FromWidth =
          TruncInst->getOperand(0)->getType()->getIntegerBitWidth();
      auto ToWidth = TruncInst->getType()->getIntegerBitWidth();

      if (FromWidth == 64) {
        if (ToWidth == 8) {
          doGreedyMemOp(SI, Opcode::I64Store8);
          WorkList.push_back(TruncInst->getOperand(0));
          return;
        }
        if (ToWidth == 16) {
          doGreedyMemOp(SI, Opcode::I64Store16);
          WorkList.push_back(TruncInst->getOperand(0));
          return;
        }
        if (ToWidth == 32) {
          doGreedyMemOp(SI, Opcode::I64Store32);
          WorkList.push_back(TruncInst->getOperand(0));
          return;
        }
      }
      if (FromWidth == 32) {
        if (ToWidth == 8) {
          doGreedyMemOp(SI, Opcode::I32Store8);
          WorkList.push_back(TruncInst->getOperand(0));
          return;
        }
        if (ToWidth == 16) {
          doGreedyMemOp(SI, Opcode::I32Store16);
          WorkList.push_back(TruncInst->getOperand(0));
          return;
        }
      }
    }

    const unsigned Width = StoreType->getIntegerBitWidth();
    // For constants, we allow 8 and 16-bit, as these are not legalized away.
    // However, we need to push it onto the worklist as a i32
    if (const auto *Const =
            llvm::dyn_cast<llvm::ConstantInt>(SI.getValueOperand())) {
      if (Width <= 8) {
        doGreedyMemOp(SI, Opcode::I32Store8);
      } else if (Width <= 16) {
        doGreedyMemOp(SI, Opcode::I32Store16);
      }
      if (Width <= 16) {
        WorkList.push_back(llvm::ConstantInt::get(
            llvm::Type::getInt32Ty(SI.getContext()), Const->getZExtValue()));
        return;
      }
    }
    if (Width == 32) {
      doGreedyMemOp(SI, Opcode::I32Store);
      WorkList.push_back(SI.getValueOperand());
      return;
    }
    if (Width == 64) {
      doGreedyMemOp(SI, Opcode::I64Store);
      WorkList.push_back(SI.getValueOperand());
      return;
    }
    WATEVER_UNREACHABLE("Cannot store integer of width {}", Width);
  }

  if (StoreType->isFloatingPointTy()) {
    if (StoreType->isDoubleTy()) {
      doGreedyMemOp(SI, Opcode::F64Store);
      WorkList.push_back(SI.getValueOperand());
      return;
    }
    if (StoreType->isFloatTy()) {
      doGreedyMemOp(SI, Opcode::F32Store);
      WorkList.push_back(SI.getValueOperand());
      return;
    }
    WATEVER_UNIMPLEMENTED("Unsupported floating point type {} for store",
                          llvmToString(*StoreType));
  }

  if (StoreType->isPointerTy()) {
    if (SI.getModule()->getDataLayout().getPointerSizeInBits() == 64) {
      doGreedyMemOp(SI, Opcode::I64Store);
    } else {
      doGreedyMemOp(SI, Opcode::I32Store);
    }
    WorkList.push_back(SI.getValueOperand());
    return;
  }
  WATEVER_UNREACHABLE("Unsupported store type {}", llvmToString(*StoreType));
}

//===----------------------------------------------------------------------===//
// Conversion Operations
//===----------------------------------------------------------------------===//
void BlockLowering::visitTruncInst(llvm::TruncInst &TI) {
  auto FromWidth = TI.getOperand(0)->getType()->getIntegerBitWidth();
  auto ToWidth = TI.getType()->getIntegerBitWidth();

  WorkList.push_back(TI.getOperand(0));
  if (FromWidth > 32 && ToWidth <= 32) {
    Actions.Insts.push_back(Opcode::I32WrapI64);
    return;
  }

  // Otherwise, truncation is a no-op
}

void BlockLowering::visitZExtInst(llvm::ZExtInst &ZI) {
  auto FromWidth = ZI.getOperand(0)->getType()->getIntegerBitWidth();
  auto ToWidth = ZI.getType()->getIntegerBitWidth();

  if (auto *LI = llvm::dyn_cast<llvm::LoadInst>(ZI.getOperand(0))) {
    // TODO use alignment
    auto *LoadType = LI->getType();
    // uint32_t Alignment = LoadInst->getAlign().value();
    assert(LoadType->isIntegerTy());
    if (ToWidth == 32) {
      if (FromWidth == 8) {
        doGreedyMemOp(*LI, Opcode::I32Load8U);
        return;
      }
      if (FromWidth == 16) {
        doGreedyMemOp(*LI, Opcode::I32Load16U);
        return;
      }
    }
    if (ToWidth == 64) {
      if (FromWidth == 8) {
        doGreedyMemOp(*LI, Opcode::I64Load8U);
        return;
      }
      if (FromWidth == 16) {
        doGreedyMemOp(*LI, Opcode::I64Load16U);
        return;
      }
      if (FromWidth == 32) {
        doGreedyMemOp(*LI, Opcode::I64Load32U);
        return;
      }
    }
    WATEVER_UNREACHABLE("unknown load from {} to {} bit", FromWidth, ToWidth);
  }

  if (FromWidth <= 32 && ToWidth > 32 && ToWidth <= 64) {
    Actions.Insts.emplace_back(Opcode::I64ExtendI32U);
  }
  WorkList.push_back(ZI.getOperand(0));
}

void BlockLowering::visitSExtInst(llvm::SExtInst &SI) {
  auto FromWidth = SI.getOperand(0)->getType()->getIntegerBitWidth();
  auto ToWidth = SI.getType()->getIntegerBitWidth();

  addOperandsToWorklist(SI.operands());

  // TODO support --enable-sign-extension
  if (FromWidth != 32 || ToWidth != 64) {
    WATEVER_UNREACHABLE("Can not expand from {} to {}", FromWidth, ToWidth);
  }

  Actions.Insts.emplace_back(Opcode::I64ExtendI32S);
}

//===----------------------------------------------------------------------===//
// Other Operations
//===----------------------------------------------------------------------===//
void BlockLowering::visitICmpInst(llvm::ICmpInst &II) {
  unsigned Width;

  if (II.getOperand(0)->getType()->isPointerTy()) {
    Width = II.getModule()->getDataLayout().getPointerSizeInBits();
  } else {
    Width = II.getOperand(0)->getType()->getIntegerBitWidth();
  }

  bool Handled = true;

  addOperandsToWorklist(II.operands());

  auto Dispatch = [&](Opcode::Enum Op32, Opcode::Enum Op64) {
    if (Width == 32)
      Actions.Insts.emplace_back(Op32);
    else if (Width == 64)
      Actions.Insts.emplace_back(Op64);
    else
      Handled = false;
  };

  switch (II.getCmpPredicate()) {
  case llvm::CmpInst::ICMP_EQ: {
    Dispatch(Opcode::I32Eq, Opcode::I64Eq);
    break;
  }
  case llvm::CmpInst::ICMP_NE: {
    Dispatch(Opcode::I32Ne, Opcode::I64Ne);
    break;
  }
  case llvm::CmpInst::ICMP_UGT: {
    Dispatch(Opcode::I32GtU, Opcode::I64GtS);
    break;
  }
  case llvm::CmpInst::ICMP_UGE: {
    Dispatch(Opcode::I32GeU, Opcode::I64GeU);
    break;
  }
  case llvm::CmpInst::ICMP_ULT: {
    Dispatch(Opcode::I32LtU, Opcode::I64LtU);
    break;
  }
  case llvm::CmpInst::ICMP_ULE: {
    Dispatch(Opcode::I32LeU, Opcode::I64LeU);
    break;
  }
  case llvm::CmpInst::ICMP_SGT: {
    Dispatch(Opcode::I32GtS, Opcode::I64GtS);
    break;
  }
  case llvm::CmpInst::ICMP_SGE: {
    Dispatch(Opcode::I32GeS, Opcode::I64GeS);
    break;
  }
  case llvm::CmpInst::ICMP_SLT: {
    Dispatch(Opcode::I32LtS, Opcode::I64LtS);
    break;
  }
  case llvm::CmpInst::ICMP_SLE: {
    Dispatch(Opcode::I32LeS, Opcode::I64LeS);
    break;
  }
  default:
    WATEVER_UNREACHABLE("Illegal int comparison");
  }

  if (!Handled) {
    WATEVER_UNREACHABLE("Unhandled ICMP");
  }
}

void BlockLowering::visitFCmpInst(llvm::FCmpInst &FI) {
  const unsigned Width = FI.getOperand(0)->getType()->getScalarSizeInBits();
  bool Handled = true;

  auto Pred = FI.getPredicate();

  if (Pred != llvm::CmpInst::FCMP_TRUE && Pred != llvm::CmpInst::FCMP_FALSE) {
    addOperandsToWorklist(FI.operands());
  }

  auto Dispatch = [&](Opcode::Enum Op32, Opcode::Enum Op64) {
    if (Width == 32)
      Actions.Insts.emplace_back(Op32);
    else if (Width == 64)
      Actions.Insts.emplace_back(Op64);
    else
      Handled = false;
  };

  switch (FI.getPredicate()) {
  case llvm::CmpInst::FCMP_FALSE: {
    Actions.Insts.emplace_back(Opcode::I32Const, 0);
    break;
  }
  case llvm::CmpInst::FCMP_OEQ: {
    Dispatch(Opcode::F32Eq, Opcode::F64Eq);
    break;
  }
  case llvm::CmpInst::FCMP_OGT: {
    Dispatch(Opcode::F32Gt, Opcode::F64Gt);
    break;
  }
  case llvm::CmpInst::FCMP_OGE: {
    Dispatch(Opcode::F32Ge, Opcode::F64Ge);
    break;
  }
  case llvm::CmpInst::FCMP_OLT: {
    Dispatch(Opcode::F32Lt, Opcode::F64Lt);
    break;
  }
  case llvm::CmpInst::FCMP_OLE: {
    Dispatch(Opcode::F32Le, Opcode::F64Le);
    break;
  }
  case llvm::CmpInst::FCMP_UNE: {
    Dispatch(Opcode::F32Ne, Opcode::F64Ne);
    break;
  }
  case llvm::CmpInst::FCMP_TRUE: {
    Actions.Insts.emplace_back(Opcode::I32Const, 1);
    break;
  }
  case llvm::CmpInst::FCMP_ORD: {
    WATEVER_UNREACHABLE("Ordered check is not supported");
  }
  case llvm::CmpInst::FCMP_UNO:
  case llvm::CmpInst::FCMP_UEQ:
  case llvm::CmpInst::FCMP_UGT:
  case llvm::CmpInst::FCMP_UGE:
  case llvm::CmpInst::FCMP_ULT:
  case llvm::CmpInst::FCMP_ULE:
  case llvm::CmpInst::FCMP_ONE:
    WATEVER_UNREACHABLE("Unordered float comparisons are not supported");
  default:
    WATEVER_UNREACHABLE("Illegal float comparison");
  }
}

void BlockLowering::visitPHINode(llvm::PHINode &PN) {
  Actions.Insts.emplace_back(Opcode::LocalGet, Parent->LocalMapping[&PN]);
}

void BlockLowering::visitSelectInst(llvm::SelectInst &SI) {
  Actions.Insts.push_back(Opcode::Select);
  WorkList.push_back(SI.getTrueValue());
  WorkList.push_back(SI.getFalseValue());
  WorkList.push_back(SI.getCondition());
}

void BlockLowering::visitCallInst(llvm::CallInst &CI) {
  auto *Callee = CI.getCalledFunction();
  if (Callee && Callee->isIntrinsic()) {
    return handleIntrinsic(CI);
  }
  addOperandsToWorklist(CI.args());

  if (Callee) {
    auto *Func = M.FunctionMap[Callee];
    Actions.Insts.emplace_back(Opcode::Call,
                               std::make_unique<RelocatableFuncArg>(Func));
  } else {
    WorkList.push_back(CI.getCalledOperand());
    auto *FuncTable = M.getIndirectFunctionTable();
    auto *FT = CI.getFunctionType();
    FuncType WasmFuncTy{};
    for (auto *ParamTy : FT->params()) {
      auto WasmType = fromLLVMType(ParamTy, CI.getModule()->getDataLayout());
      WasmFuncTy.Params.push_back(WasmType);
    }

    if (!FT->getReturnType()->isVoidTy()) {
      WasmFuncTy.Results.push_back(
          fromLLVMType(FT->getReturnType(), CI.getModule()->getDataLayout()));
    }

    const uint32_t FuncTypeIndex = M.getOrAddType(WasmFuncTy);

    Actions.Insts.emplace_back(
        Opcode::CallIndirect,
        std::make_unique<RelocatableIndirectCallArg>(FuncTypeIndex, FuncTable));
  }
}

std::unique_ptr<WasmActions> BlockLowering::lower() {
  WATEVER_LOG_TRACE("Lowering {}", BB->getName().str());

  auto Result = std::make_unique<WasmActions>();

  for (auto &Inst : *BB) {
    // For each instructions with possible side effects, build a dependency tree
    // on the stack in reverse order.
    if (Inst.mayHaveSideEffects() || Inst.isTerminator() ||
        hasExternalUser(&Inst)) {
      WATEVER_LOG_TRACE("{} is AST root, materialize", llvmToString(Inst));
      llvm::DenseMap<llvm::Value *, Local *> ASTLocals;
      WorkList.push_back(&Inst);
      auto Counts = getDependencyTreeUserCount(&Inst);

      while (!WorkList.empty()) {
        auto *Next = WorkList.pop_back_val();
        WATEVER_LOG_TRACE("handling {}", llvmToString(*Next));
        // Check if an earlier instrucion has already produced this value
        // outside this AST (e.g., we might be the second user)
        if (auto It = Parent->LocalMapping.find(Next);
            It != Parent->LocalMapping.end()) {
          Actions.Insts.emplace_back(Opcode::LocalGet, It->second);
          WATEVER_LOG_TRACE("has already a local, loading");
          continue;
        }

        // There will come an instruction in the worklist materializing Next
        if (Counts[Next] > 1) {
          WATEVER_LOG_TRACE("will get materialized later");
          Counts[Next]--;
          // We already have a local for this
          if (auto It = ASTLocals.find(Next); It != ASTLocals.end()) {
            Actions.Insts.emplace_back(Opcode::LocalGet, It->second);
            continue;
          }
          // Create a local
          auto *L = Parent->getNewLocal(
              fromLLVMType(Next->getType(), BB->getDataLayout()));
          Actions.Insts.emplace_back(Opcode::LocalGet, L);
          ASTLocals[Next] = L;
          continue;
        }

        // Next needs to be materialized
        if (auto *Inst = llvm::dyn_cast<llvm::Instruction>(Next)) {
          // Save to local, if we have a later user
          if (Inst->getNumUses() > 1) {
            Local *L;
            if (auto It = ASTLocals.find(Next); It != ASTLocals.end()) {
              L = It->second;
            } else {
              L = Parent->getNewLocal(
                  fromLLVMType(Next->getType(), BB->getDataLayout()));
            }
            Actions.Insts.emplace_back(Opcode::LocalTee, L);
            Parent->LocalMapping[Next] = L;
          }
          visit(*Inst);
        } else if (auto *GV = llvm::dyn_cast<llvm::GlobalVariable>(Next)) {
          auto *WasmData = M.DataMap[GV];
          if (!WasmData) {
            WATEVER_UNREACHABLE("unknown global {}", llvmToString(*GV));
          }
          if (BB->getDataLayout().getPointerSizeInBits() == 64) {
            Actions.Insts.emplace_back(Opcode::I64Const, WasmData);
          } else {
            Actions.Insts.emplace_back(Opcode::I32Const, WasmData);
          }
        }
        // Otherwise, we can just load constants/arguments
        else if (const auto *Const = llvm::dyn_cast<llvm::ConstantInt>(Next)) {
          if (Const->getBitWidth() == 1) {
            Actions.Insts.push_back(
                WasmInst(Opcode::I32Const, Const->getValue().getZExtValue()));
          } else if (Const->getBitWidth() == 32) {
            Actions.Insts.push_back(
                WasmInst(Opcode::I32Const, Const->getValue().getSExtValue()));
          } else if (Const->getBitWidth() == 64) {
            Actions.Insts.push_back(
                WasmInst(Opcode::I64Const, Const->getValue().getSExtValue()));
          } else {
            WATEVER_UNREACHABLE("unsupported constant bit width: {}",
                                Const->getBitWidth());
          }
        } else if (const auto *FConst =
                       llvm::dyn_cast<llvm::ConstantFP>(Next)) {
          if (FConst->getType()->isFloatTy()) {
            WATEVER_TODO("put float {} on top of the stack",
                         FConst->getValue().convertToFloat());
          } else if (FConst->getType()->isDoubleTy()) {
            WATEVER_TODO("put double {} on top of the stack",
                         FConst->getValue().convertToDouble());
          }
        } else if (auto *Arg = llvm::dyn_cast<llvm::Argument>(Next)) {
          Actions.Insts.push_back(WasmInst(Opcode::LocalGet, Arg->getArgNo()));
        } else if (auto *F = llvm::dyn_cast<llvm::Function>(Next)) {
          auto *WasmFunc = M.FunctionMap[F];
          M.addIndirectFunctionElement(WasmFunc);
          auto Arg = std::make_unique<RelocatableTableIndexArg>(WasmFunc);
          if (F->getDataLayout().getPointerSizeInBits() == 64) {
            Actions.Insts.emplace_back(Opcode::I64Const, std::move(Arg));
          } else {
            Actions.Insts.emplace_back(Opcode::I32Const, std::move(Arg));
          }
        } else if (llvm::isa<llvm::ConstantPointerNull>(Next)) {
          if (BB->getDataLayout().getPointerSizeInBits() == 64) {
            Actions.Insts.emplace_back(Opcode::I64Const, 0);
          } else {
            Actions.Insts.emplace_back(Opcode::I32Const, 0);
          }
        } else {
          WATEVER_TODO("put {} on top of the stack", llvmToString(*Next));
        }
      }
      // TODO the value might not have side effects, but may still be used in
      // another BB. We then have to assign a local too.
      std::ranges::reverse(Actions.Insts);
      Result->Insts.insert(Result->Insts.end(),
                           std::make_move_iterator(Actions.Insts.begin()),
                           std::make_move_iterator(Actions.Insts.end()));
      Actions.Insts.clear();
      for (auto &[Val, Loc] : ASTLocals) {
        Parent->LocalMapping[Val] = Loc;
      }

      // If the instruction has users, ensure that is in a local, so it will
      // never be materialized as a dependency again
      if (Inst.getNumUses() > 0) {
        auto *L = Parent->getNewLocal(
            fromLLVMType(Inst.getType(), BB->getDataLayout()));
        Parent->LocalMapping[&Inst] = L;
        Result->Insts.emplace_back(Opcode::LocalSet, L);
      } else {
        if (!Inst.getType()->isVoidTy()) {
          Actions.Insts.emplace_back(Opcode::Drop);
        }
      }
    }
  }

  return Result;
}

std::unique_ptr<Wasm> FunctionLowering::doBranch(const llvm::BasicBlock *Source,
                                                 llvm::BasicBlock *Target,
                                                 Context Ctx) {

  // Actions to be executed on the edge
  WasmActions PhiActions;
  for (auto &Phi : Target->phis()) {
    WATEVER_LOG_TRACE("emitting phi edge from {} to {}", getBlockName(Source),
                      getBlockName(Target));

    llvm::Value *IncomingVal = Phi.getIncomingValueForBlock(Source);
    WATEVER_LOG_TRACE("incoming value is {}", llvmToString(*IncomingVal));
    WATEVER_LOG_TRACE("PHI node is: {}", llvmToString(Phi));

    if (auto *Arg = llvm::dyn_cast<llvm::Argument>(IncomingVal)) {
      PhiActions.Insts.push_back(WasmInst(Opcode::LocalGet, Arg->getArgNo()));
    } else {
      Local *SourceLocal;
      if (auto It = F->LocalMapping.find(IncomingVal);
          It != F->LocalMapping.end()) {
        // The source has already been emitted
        SourceLocal = It->second;
      } else {
        // Or maybe it has not been emitted yet, so register a new local for it
        // (e.g., loop header
        SourceLocal = F->getNewLocal(
            fromLLVMType(IncomingVal->getType(), Source->getDataLayout()));
        F->LocalMapping[IncomingVal] = SourceLocal;
      }
      PhiActions.Insts.push_back(WasmInst(Opcode::LocalGet, SourceLocal));
    }

    Local *DestLocal;
    if (auto It = F->LocalMapping.find(&Phi); It != F->LocalMapping.end()) {
      DestLocal = It->second;
    } else {
      DestLocal =
          F->getNewLocal(fromLLVMType(Phi.getType(), Target->getDataLayout()));
      F->LocalMapping[&Phi] = DestLocal;
    }

    WATEVER_LOG_TRACE("local set {}", DestLocal->Index);
    PhiActions.Insts.push_back(WasmInst(Opcode::LocalSet, DestLocal));
  }

  std::unique_ptr<Wasm> BranchNode;

  // Backward branch (continue) or forward branch (exit)
  if (DT.dominates(Target, Source) || isMergeNode(Target)) {
#ifdef WATEVER_LOGGING
    if (DT.dominates(Target, Source)) {
      WATEVER_LOG_TRACE("backwards branch from {} to {}", getBlockName(Source),
                        getBlockName(Target));
    } else {
      WATEVER_LOG_TRACE("forwards branch from {} to {}", getBlockName(Source),
                        getBlockName(Target));
    }
#endif
    BranchNode = std::make_unique<WasmBr>(WasmBr{index(Target, Ctx)});
  } else {

    WATEVER_LOG_TRACE("no branch needed from {} to {}, fall through",
                      getBlockName(Source), getBlockName(Target));
    BranchNode = doTree(Target, Ctx);
  }

  if (!PhiActions.Insts.empty()) {
    return std::make_unique<WasmSeq>(
        std::make_unique<WasmActions>(std::move(PhiActions)),
        std::move(BranchNode));
  }
  return BranchNode;
}

std::unique_ptr<Wasm> FunctionLowering::doTree(llvm::BasicBlock *Root,
                                               Context Ctx) {
  WATEVER_LOG_TRACE("doTree with root {}", getBlockName(Root));

  llvm::SmallVector<llvm::BasicBlock *> MergeChildren;
  getMergeChildren(Root, MergeChildren);

  // Emit loop block
  if (LI.isLoopHeader(Root)) {
    WATEVER_LOG_TRACE("Generating loop for {}", getBlockName(Root));
    Ctx.push_back(ContainingSyntax::createLoop(Root));
    return std::make_unique<WasmLoop>(
        WasmLoop{nodeWithin(Root, MergeChildren, Ctx)});
  }

  return nodeWithin(Root, MergeChildren, Ctx);
}

std::unique_ptr<Wasm> FunctionLowering::nodeWithin(
    llvm::BasicBlock *Parent,
    llvm::SmallVector<llvm::BasicBlock *> MergeChildren, const Context &Ctx) {

  // Base case
  if (MergeChildren.empty()) {
    auto Body = translateBB(Parent);

    std::unique_ptr<Wasm> Leaving;
    auto *Term = Parent->getTerminator();
    if (auto *Br = llvm::dyn_cast<llvm::BranchInst>(Term)) {
      if (Br->isConditional()) {
        assert(Br->getNumSuccessors() == 2 &&
               "expected two successors in conditional branch");

        WATEVER_LOG_TRACE("{} branches to {} and {}", getBlockName(Parent),
                          getBlockName(Br->getSuccessor(0)),
                          getBlockName(Br->getSuccessor(1)));

        Leaving = std::make_unique<WasmIf>(
            doBranch(Parent, Br->getSuccessor(0), Ctx),
            doBranch(Parent, Br->getSuccessor(1), Ctx));
      } else {
        assert(Br->getNumSuccessors() == 1 &&
               "expected only one successor in unconditional branch");

        WATEVER_LOG_TRACE("{} branches to {}", getBlockName(Parent),
                          getBlockName(Br->getSuccessor(0)));

        Leaving = doBranch(Parent, Br->getSuccessor(0), Ctx);
      }
    } else if (llvm::isa<llvm::ReturnInst>(Term)) {
      Leaving = std::make_unique<WasmReturn>();
    } else if (llvm::isa<llvm::UnreachableInst>(Term)) {
      WasmActions UnreachableAction;
      UnreachableAction.Insts.emplace_back(Opcode::Unreachable);
      Leaving = std::make_unique<WasmActions>(std::move(UnreachableAction));
    } else {
      // TODO support switch
      WATEVER_UNREACHABLE("unsupported terminator: {}",
                          Parent->getTerminator()->getOpcodeName());
    }

    return std::make_unique<WasmSeq>(
        WasmSeq{std::move(Body), std::move(Leaving)});
  }

  auto *Follower = MergeChildren.back();
  MergeChildren.pop_back();

  WATEVER_LOG_TRACE("{} is followed by {}", getBlockName(Parent),
                    getBlockName(Follower));

  auto FirstContext = Ctx;
  FirstContext.push_back(ContainingSyntax::createBlock(Follower));

  auto First = std::make_unique<WasmBlock>(
      WasmBlock{nodeWithin(Parent, MergeChildren, FirstContext)});

  auto Second = doTree(Follower, Ctx);

  return std::make_unique<WasmSeq>(
      WasmSeq{std::move(First), std::move(Second)});
}

int FunctionLowering::index(const llvm::BasicBlock *BB, Context &Ctx) {
  int I = 0;
  // Iterate in reverse to find the innermost matching label (relative index 0)
  for (auto &It : std::ranges::reverse_view(Ctx)) {
    if (It.Label == BB) {
      return I;
    }
    ++I;
  }
  WATEVER_UNREACHABLE("unknown branch target");
}

std::unique_ptr<WasmActions>
FunctionLowering::translateBB(llvm::BasicBlock *BB) const {
  BlockLowering BL{BB, M, F};
  // Just create a local, so return instructions know where to find it. Prolog
  // will be patched in, after the function has been handled
  if (!F->SavedSP) {
    bool HasAlloca = std::ranges::any_of(*BB, [](const llvm::Instruction &I) {
      return llvm::isa<llvm::AllocaInst>(I);
    });
    if (HasAlloca) {
      auto PointerType =
          BB->getModule()->getDataLayout().getPointerSizeInBits() == 64
              ? ValType::I64
              : ValType::I32;

      F->SavedSP = F->getNewLocal(PointerType);
    }
  }

  return BL.lower();
}

void FunctionLowering::getMergeChildren(
    const llvm::BasicBlock *R,
    llvm::SmallVectorImpl<llvm::BasicBlock *> &Result) const {
  if (auto *Node = DT.getNode(R)) {
    for (const auto *Child : *Node) {
      if (isMergeNode(Child->getBlock())) {
        WATEVER_LOG_TRACE("{} is dominated merge",
                          getBlockName(Child->getBlock()));
        Result.push_back(Child->getBlock());
      }
    }
  }
}

Module ModuleLowering::convert(llvm::Module &Mod,
                               llvm::FunctionAnalysisManager &FAM) {
  Module Res{};

  uint32_t FunctionIndexCounter = 0;

  for (auto &GV : Mod.globals()) {
    WATEVER_LOG_TRACE("Adding global value {}", GV.getName().str());

    if (GV.isDeclaration()) {
      auto UndefData = std::make_unique<UndefinedData>(Res.Symbols.size(),
                                                       GV.getName().str());
      UndefData->setFlag(SymbolFlag::WASM_SYM_UNDEFINED);
      Res.DataMap[&GV] = UndefData.get();
      Res.Symbols.push_back(std::move(UndefData));
      continue;
    }

    std::vector<uint8_t> Content;
    llvm::SmallVector<RelocationEntry> Relocs;

    Res.flattenConstant(GV.getInitializer(), Content, Relocs);

    DataSection S;
    if (GV.isConstant()) {
      S = DataSection::RO_DATA;
    } else if (GV.getInitializer()->isNullValue() ||
               GV.getInitializer()->isZeroValue()) {
      S = DataSection::BSS;
    } else {
      S = DataSection::DATA;
    }

    auto DefData = std::make_unique<DefinedData>(
        Res.Symbols.size(), Res.Datas.size(), true, GV.getName().str(), Content,
        Relocs, S);

    if (GV.getThreadLocalMode() != llvm::GlobalValue::NotThreadLocal) {
      DefData->setFlag(SegmentFlag::WASM_SEGMENT_FLAG_TLS);
    }
    // TODO check if string or retain flags needed
    // TODO set alignment
    Res.DataMap[&GV] = DefData.get();
    Res.Datas.push_back(DefData.get());
    Res.Symbols.push_back(std::move(DefData));
  }

  // Imported functions must be defined first
  for (auto &F : Mod) {
    // Intrinsics will not get imported, nor called - but directly lowered in
    // place
    if (!F.isDeclaration() || F.isIntrinsic()) {
      continue;
    }
    auto *FT = F.getFunctionType();
    FuncType WasmFuncTy{};
    for (auto *ParamTy : FT->params()) {
      auto WasmType = fromLLVMType(ParamTy, Mod.getDataLayout());
      WasmFuncTy.Params.push_back(WasmType);
    }

    if (!FT->getReturnType()->isVoidTy()) {
      WasmFuncTy.Results.push_back(
          fromLLVMType(FT->getReturnType(), Mod.getDataLayout()));
    }

    const uint32_t FuncTypeIndex = Res.getOrAddType(WasmFuncTy);
    auto Import = std::make_unique<ImportedFunc>(
        Res.Symbols.size(), FuncTypeIndex, FunctionIndexCounter++,
        F.getName().str());
    Import->setFlag(SymbolFlag::WASM_SYM_UNDEFINED);
    Res.FunctionMap[&F] = Import.get();
    Res.Imports.push_back(Import.get());
    Res.Symbols.push_back(std::move(Import));
  }

  // Then declare all defined functions. This needs to be done before lowering,
  // so we can use not-yet-lowered functions (e.g., in calls).
  for (auto &F : Mod) {
    if (F.isDeclaration()) {
      continue;
    }

    auto *FT = F.getFunctionType();

    FuncType WasmFuncTy{};
    for (auto *ParamTy : FT->params()) {
      auto WasmType = fromLLVMType(ParamTy, Mod.getDataLayout());
      WasmFuncTy.Params.push_back(WasmType);
    }

    if (!FT->getReturnType()->isVoidTy()) {
      WasmFuncTy.Results.push_back(
          fromLLVMType(FT->getReturnType(), Mod.getDataLayout()));
    }

    const uint32_t FuncTypeIndex = Res.getOrAddType(WasmFuncTy);

    auto FunctionPtr = std::make_unique<DefinedFunc>(
        Res.Symbols.size(), FuncTypeIndex, FunctionIndexCounter++,
        static_cast<uint32_t>(F.arg_size()), F.getName());

    // TODO use correct flags
    FunctionPtr->setFlag(SymbolFlag::WASM_SYM_VISIBILITY_HIDDEN);
    Res.FunctionMap[&F] = FunctionPtr.get();
    Res.Functions.push_back(FunctionPtr.get());
    Res.Symbols.push_back(std::move(FunctionPtr));
  }

  // Lower functions, which might involve adding a prolog to save the SP
  for (auto &F : Mod) {
    if (F.isDeclaration()) {
      continue;
    }
    auto *WasmFunc = static_cast<DefinedFunc *>(Res.FunctionMap[&F]);
    auto &DT = FAM.getResult<llvm::DominatorTreeAnalysis>(F);
    auto &LI = FAM.getResult<llvm::LoopAnalysis>(F);

    FunctionLowering FL{WasmFunc, DT, LI, Res};
    WATEVER_LOG_DBG("Lowering function {}", F.getName().str());
    FL.lower();

    if (WasmFunc->SavedSP) {
      // Insert prolog to save SP
      auto Prolog = std::make_unique<WasmActions>();
      if (!Res.StackPointer) {
        WATEVER_LOG_WARN("Unset SP");
      }
      if (!WasmFunc->SavedSP) {
        WATEVER_LOG_WARN("Unsset SavedSP");
      }
      auto GlobalArg = std::make_unique<RelocatableGlobalArg>(Res.StackPointer);
      Prolog->Insts.emplace_back(Opcode::GlobalGet, std::move(GlobalArg));
      Prolog->Insts.emplace_back(Opcode::LocalSet, WasmFunc->SavedSP);
      WasmFunc->Body = std::make_unique<WasmSeq>(std::move(Prolog),
                                                 std::move(WasmFunc->Body));
    }

    dumpWasm(*WasmFunc->Body);
  }
  return Res;
}
