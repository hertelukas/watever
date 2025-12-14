#include "watever/ir.hpp"
#include "watever/opcode.hpp"
#include "watever/printer.hpp"
#include "watever/utils.hpp"
#include <algorithm>
#include <llvm/ADT/DenseMap.h>
#include <llvm/Analysis/LoopNestAnalysis.h>
#include <llvm/IR/Argument.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/Casting.h>
#include <memory>

using namespace watever;

void BlockLowering::calculateLiveOut() {
  for (auto &Val : *BB) {
    if (Val.isTerminator()) {
      WorkList.push_back(&Val);
      continue;
    }
    if (Val.mayHaveSideEffects()) {
      WorkList.push_back(&Val);
      continue;
    }
    if (Val.getNumUses() == 0) {
      continue;
    }
    for (auto *User : Val.users()) {
      if (auto *Inst = llvm::dyn_cast<llvm::Instruction>(User)) {
        // value is used outside of this basic block
        if (Inst->getParent() != BB) {
          WorkList.push_back(&Val);
          break;
        }
      }
    }
  }
}

llvm::DenseMap<llvm::Value *, int>
BlockLowering::getInternalUserCounts() const {
  // TODO think about PHI nodes
  llvm::DenseMap<llvm::Value *, int> Result;

  for (auto &Inst : *BB) {
    for (auto *User : Inst.users()) {
      if (auto *UserInst = llvm::dyn_cast<llvm::Instruction>(User)) {
        if (UserInst->getParent() == BB) {
          Result[&Inst]++;
        }
      }
    }
  }

  return Result;
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
  const auto Width = Ty->getPrimitiveSizeInBits();
  bool Handled = true;

  addOperandsToWorklist(BO.operands());

  auto Dispatch = [&](Opcode::Enum Op32, Opcode::Enum Op64) {
    if (Width == 32)
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
    WATEVER_TODO("lowering of {}", BO.getOpcodeName());
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

  // TODO schedule value
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

  if (FromWidth == 32 && ToWidth == 64) {
    Actions.Insts.emplace_back(Opcode::I64ExtendI32U);
  }
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
  const unsigned Width = II.getOperand(0)->getType()->getIntegerBitWidth();
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

  addOperandsToWorklist(FI.operands());

  auto Dispatch = [&](Opcode::Enum Op32, Opcode::Enum Op64) {
    if (Width == 32)
      Actions.Insts.emplace_back(Op32);
    else if (Width == 64)
      Actions.Insts.emplace_back(Op64);
    else
      Handled = false;
  };

  switch (FI.getPredicate()) {
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
  case llvm::CmpInst::FCMP_ONE: {
    Dispatch(Opcode::F32Ne, Opcode::F64Ne);
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
  case llvm::CmpInst::FCMP_UNE:
    WATEVER_UNREACHABLE("Unordered float comparisons are not supported");
  default:
    WATEVER_UNREACHABLE("Illegal float comparison");
  }
}

void BlockLowering::visitSelectInst(llvm::SelectInst &SI) {
  Actions.Insts.push_back(Opcode::Select);
  WorkList.push_back(SI.getTrueValue());
  WorkList.push_back(SI.getFalseValue());
  WorkList.push_back(SI.getCondition());
}

void BlockLowering::visitCallInst(llvm::CallInst &CI) {
  auto *Callee = CI.getCalledFunction();
  addOperandsToWorklist(CI.args());

  if (Callee) {
    auto *Func = M.FunctionMap[Callee];
    Actions.Insts.emplace_back(Opcode::Call, std::make_unique<CallArg>(Func));
  }
}

std::unique_ptr<WasmActions> BlockLowering::lower(Function *F) {
  WATEVER_LOG_TRACE("Lowering {}", BB->getName().str());
  // The last "live-out" value is handled first, ensuring that all live-out
  // values are emitted in the correct order - especially stores.
  calculateLiveOut();

#ifdef WATEVER_LOGGING
  WATEVER_LOG_TRACE("with live-out values:");
  for (auto *I : WorkList) {
    WATEVER_LOG_TRACE("{}", llvmToString(*I));
  }
#endif // WATEVER_LOGGING

  auto Count = getInternalUserCounts();

  while (!WorkList.empty()) {
    auto *Next = WorkList.back();
    WorkList.pop_back();

    WATEVER_LOG_TRACE("{} is needed by {} instruction(s), including ourselves",
                      llvmToString(*Next), Count[Next]);

    if (Count.lookup(Next) <= 1) {
      WATEVER_LOG_TRACE("...materializing");
      // Check, if someone already gets this value. If so, we tee it
      if (auto It = LocalMapping.find(Next); It != LocalMapping.end()) {
        Actions.Insts.push_back(WasmInst(Opcode::LocalTee, It->second));
      }
      // If next is an instruction, lower it, and push args on the
      // WorkList stack
      if (auto *Inst = llvm::dyn_cast<llvm::Instruction>(Next)) {
        visit(*Inst);
      }
      // Otherwise, we can just load constants/arguments
      else if (const auto *Const = llvm::dyn_cast<llvm::ConstantInt>(Next)) {
        if (Const->getBitWidth() == 32) {
          Actions.Insts.push_back(
              WasmInst(Opcode::I32Const, Const->getValue().getZExtValue()));
        } else if (Const->getBitWidth() == 64) {
          Actions.Insts.push_back(
              WasmInst(Opcode::I64Const, Const->getValue().getZExtValue()));
        } else {
          WATEVER_UNREACHABLE("unsupported constant bit width: {}",
                              Const->getBitWidth());
        }
      } else if (const auto *FConst = llvm::dyn_cast<llvm::ConstantFP>(Next)) {
        if (FConst->getType()->isFloatTy()) {
          WATEVER_TODO("put float {} on top of the stack",
                       FConst->getValue().convertToFloat());
        } else if (FConst->getType()->isDoubleTy()) {
          WATEVER_TODO("put double {} on top of the stack",
                       FConst->getValue().convertToDouble());
        }
      } else if (auto *Arg = llvm::dyn_cast<llvm::Argument>(Next)) {
        Actions.Insts.push_back(WasmInst(Opcode::LocalGet, Arg->getArgNo()));
      } else {
        WATEVER_TODO("put {} on top of the stack", llvmToString(*Next));
      }
    } else {
      WATEVER_LOG_TRACE("... so we can just load it");
      Count[Next]--;
      LocalArg *Local;
      // Check if someone else is already getting it
      if (auto It = LocalMapping.find(Next); It != LocalMapping.end()) {
        Local = It->second;
      } else {
        // If not, we create it
        Local = F->getNewLocal(
            Type::fromLLVMType(Next->getType(), BB->getDataLayout()));
      }
      LocalMapping[Next] = Local;
      Actions.Insts.push_back(WasmInst(Opcode::LocalGet, Local));
    }
  }

  std::reverse(Actions.Insts.begin(), Actions.Insts.end());
  return std::make_unique<WasmActions>(std::move(Actions));
}

std::unique_ptr<Wasm> FunctionLowering::doBranch(const llvm::BasicBlock *Source,
                                                 llvm::BasicBlock *Target,
                                                 Context Ctx) {

  // Backward branch (continue) or forward branch (exit)
  if (DT.dominates(Target, Source) || isMergeNode(Target)) {
#ifdef WATEVER_LOGGING
    if (DT.dominates(Target, Source)) {
      WATEVER_LOG_TRACE("backwards branch from {} to {}",
                        Source->getName().str(), Target->getName().str());
    } else {
      WATEVER_LOG_TRACE("forwards branch from {} to {}",
                        Source->getName().str(), Target->getName().str());
    }
#endif
    return std::make_unique<WasmBr>(WasmBr{index(Target, Ctx)});
  }

  WATEVER_LOG_TRACE("no branch needed from {} to {}, fall through",
                    Source->getName().str(), Target->getName().str());
  return doTree(Target, Ctx);
}

std::unique_ptr<Wasm> FunctionLowering::doTree(llvm::BasicBlock *Root,
                                               Context Ctx) {
  WATEVER_LOG_TRACE("doTree with root {}", Root->getName().str());

  llvm::SmallVector<llvm::BasicBlock *> MergeChildren;
  getMergeChildren(Root, MergeChildren);

  // Emit loop block
  if (LI.isLoopHeader(Root)) {
    WATEVER_LOG_TRACE("Generating loop for {}", Root->getName().str());
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

        WATEVER_LOG_TRACE("{} branches to {} and {}", Parent->getName().str(),
                          Br->getSuccessor(0)->getName().str(),
                          Br->getSuccessor(1)->getName().str());

        Leaving = std::make_unique<WasmIf>(
            doBranch(Parent, Br->getSuccessor(0), Ctx),
            doBranch(Parent, Br->getSuccessor(1), Ctx));
      } else {
        assert(Br->getNumSuccessors() == 1 &&
               "expected only one successor in unconditional branch");

        WATEVER_LOG_TRACE("{} branches to {}", Parent->getName().str(),
                          Br->getSuccessor(0)->getName().str());

        Leaving = doBranch(Parent, Br->getSuccessor(0), Ctx);
      }
    } else if (llvm::isa<llvm::ReturnInst>(Term)) {
      Leaving = std::make_unique<WasmReturn>();
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

  WATEVER_LOG_TRACE("{} is followed by {}", Parent->getName().str(),
                    Follower->getName().str());

  auto FirstContext = Ctx;
  FirstContext.push_back(ContainingSyntax::createBlock(Follower));

  auto First = std::make_unique<WasmBlock>(
      WasmBlock{nodeWithin(Parent, MergeChildren, FirstContext)});

  auto Second = doTree(Follower, Ctx);

  return std::make_unique<WasmSeq>(
      WasmSeq{std::move(First), std::move(Second)});
}

// TODO this might be wrong, needs double checking
int FunctionLowering::index(const llvm::BasicBlock *BB, Context &Ctx) {
  int I = 0;
  for (const auto &Syntax : Ctx) {
    if (Syntax.Label == BB) {
      return I;
    }
    ++I;
  }
  WATEVER_UNREACHABLE("unknown branch target");
}

std::unique_ptr<WasmActions>
FunctionLowering::translateBB(llvm::BasicBlock *BB) const {
  BlockLowering BL{BB, M};
  return BL.lower(F);
}

void FunctionLowering::getMergeChildren(
    const llvm::BasicBlock *R,
    llvm::SmallVectorImpl<llvm::BasicBlock *> &Result) const {
  if (auto *Node = DT.getNode(R)) {
    for (const auto *Child : *Node) {
      if (isMergeNode(Child->getBlock())) {
        WATEVER_LOG_TRACE("{} is dominated merge",
                          Child->getBlock()->getName().str());
        Result.push_back(Child->getBlock());
      }
    }
  }
}

Module ModuleLowering::convert(llvm::Module &Mod,
                               llvm::FunctionAnalysisManager &FAM) {
  Module Res{};
  for (auto &F : Mod) {
    if (F.isDeclaration()) {
      WATEVER_TODO("handle function declaration");
      continue;
    }

    auto *FT = F.getFunctionType();

    FuncType WasmFuncTy{};
    for (auto *ParamTy : FT->params()) {
      auto WasmType = Type::fromLLVMType(ParamTy, Mod.getDataLayout());
      WasmFuncTy.Args.push_back(WasmType);
    }

    if (!FT->getReturnType()->isVoidTy()) {
      WasmFuncTy.Results.push_back(
          Type::fromLLVMType(FT->getReturnType(), Mod.getDataLayout()));
    }

    SubType WasmTy(WasmFuncTy);
    const auto *WasmFuncTyPtr = Res.getOrAddType(WasmTy);

    auto FunctionPtr = std::make_unique<Function>(
        WasmFuncTyPtr, static_cast<uint32_t>(F.arg_size()), F.getName());

    auto *FunctionPtrPtr = FunctionPtr.get();
    Res.Functions.push_back(std::move(FunctionPtr));

    Res.FunctionMap[&F] = FunctionPtrPtr;
  }

  for (auto &F : Mod) {
    auto *WasmFunc = Res.FunctionMap[&F];
    auto &DT = FAM.getResult<llvm::DominatorTreeAnalysis>(F);
    auto &LI = FAM.getResult<llvm::LoopAnalysis>(F);

    FunctionLowering FL{WasmFunc, DT, LI, Res};
    FL.lower();
    WATEVER_LOG_DBG("Lowered function {}", F.getName().str());
    dumpWasm(*WasmFunc->Body);
  }
  return Res;
}
