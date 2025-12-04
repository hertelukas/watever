#include "watever/ir.h"
#include "watever/opcode.h"
#include "watever/utils.h"
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

llvm::SmallVector<llvm::Value *> BlockLowering::getLiveOut() const {
  llvm::SmallVector<llvm::Value *> Result;

  for (auto &Val : *BB) {
    if (Val.getOpcode() == llvm::Instruction::Store) {
      Result.push_back(&Val);
      continue;
    }
    if (Val.getOpcode() == llvm::Instruction::Ret) {
      Result.push_back(&Val);
      continue;
    }
    if (Val.getNumUses() == 0) {
      continue;
    }
    for (auto *User : Val.users()) {
      if (auto *Inst = llvm::dyn_cast<llvm::Instruction>(User)) {
        // value is used outside of this basic block
        if (Inst->getParent() != BB) {
          Result.push_back(&Val);
          break;
        }
      }
    }
  }
  return Result;
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

void BlockLowering::visitBinaryOperator(llvm::BinaryOperator &BO) {
  const auto *Ty = BO.getType();
  // TODO handle vectors
  const auto Width = Ty->getPrimitiveSizeInBits();
  bool Handled = true;

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

void BlockLowering::visitSExtInst(llvm::SExtInst &SI) {
  auto FromWidth = SI.getOperand(0)->getType()->getIntegerBitWidth();
  auto ToWidth = SI.getType()->getIntegerBitWidth();

  // TODO support --enable-sign-extension
  if (FromWidth != 32 || ToWidth != 64) {
    WATEVER_UNREACHABLE("Can not expand from {} to {}", FromWidth, ToWidth);
  }

  Actions.Insts.emplace_back(Opcode::I64Extend32S);
}

void BlockLowering::visitUnaryOperator(llvm::UnaryOperator &UO) {
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

std::unique_ptr<WasmActions> BlockLowering::lower(Function &F) {
  WATEVER_LOG_TRACE("Lowering {}", BB->getName().str());
  // The last "live-out" value is handled first, ensuring that all live-out
  // values are emitted in the correct order - especially stores.
  auto ToHandle = getLiveOut();

#ifdef WATEVER_LOGGING
  WATEVER_LOG_TRACE("with live-out values:");
  for (auto *I : ToHandle) {
    WATEVER_LOG_TRACE("{}", llvmToString(*I));
  }
#endif // WATEVER_LOGGING

  auto Count = getInternalUserCounts();
  const auto OriginalCount = Count;

  if (const auto *Br = llvm::dyn_cast<llvm::BranchInst>(BB->getTerminator())) {
    if (Br->isConditional()) {
      if (auto *Inst = llvm::dyn_cast<llvm::Instruction>(Br->getOperand(0))) {
        ToHandle.push_back(Inst);
      } else {
        WATEVER_TODO(
            "put {} on top of the stack - required for conditional branch",
            llvmToString(*Br->getOperand(0)));
      }
    }
  }

  while (!ToHandle.empty()) {
    auto *Next = ToHandle.back();
    ToHandle.pop_back();

    WATEVER_LOG_TRACE("{} is needed by {} instruction(s), including ourselves",
                      llvmToString(*Next), Count[Next]);

    if (Count.lookup(Next) <= 1) {
      WATEVER_LOG_TRACE("materializing...");
      // Check, if someone already gets this value. If so, we tee it
      if (OriginalCount.lookup(Next) > 1) {
        Actions.Insts.push_back(
            WasmInst(Opcode::LocalTee, LocalMapping.lookup(Next)));
      }
      // If next is an instruction, lower it, and push args on the
      // ToHandle stack
      if (auto *Inst = llvm::dyn_cast<llvm::Instruction>(Next)) {
        visit(*Inst);
        for (llvm::Value *Op : Inst->operands()) {
          ToHandle.push_back(Op);
        }
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
      } else if (auto *Arg = llvm::dyn_cast<llvm::Argument>(Next)) {
        Actions.Insts.push_back(WasmInst(Opcode::LocalGet, Arg->getArgNo()));
      } else {
        WATEVER_TODO("put {} on top of the stack", llvmToString(*Next));
      }
    } else {
      WATEVER_LOG_TRACE("... so we can just load it");
      Count[Next]--;
      // Generate a new local
      const auto Local = F.getNewLocal();
      LocalMapping[Next] = Local;
      Actions.Insts.push_back(WasmInst(Opcode::LocalGet, Local));
    }
  }

  std::reverse(Actions.Insts.begin(), Actions.Insts.end());
  return std::make_unique<WasmActions>(Actions);
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
  BlockLowering BL{BB};
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
    auto &DT = FAM.getResult<llvm::DominatorTreeAnalysis>(F);
    auto &LI = FAM.getResult<llvm::LoopAnalysis>(F);

    auto *FT = F.getFunctionType();

    FuncType WasmFuncTy{};
    for (auto *ParamTy : FT->params()) {
      WasmFuncTy.Args.push_back(Type::fromLLVMType(ParamTy));
    }

    if (!FT->getReturnType()->isVoidTy()) {
      WasmFuncTy.Results.push_back(Type::fromLLVMType(FT->getReturnType()));
    }

    SubType WasmTy(WasmFuncTy);
    const auto *WasmFuncTyPtr = Res.getOrAddType(WasmTy);

    Function WasmFunction{static_cast<int>(F.arg_size()), WasmFuncTyPtr};
    FunctionLowering FL{WasmFunction, DT, LI};
    FL.lower();
    WasmPrinter Printer{};
    WasmFunction.visit(Printer);
    Res.Functions.push_back(std::move(WasmFunction));
  }
  return Res;
}
