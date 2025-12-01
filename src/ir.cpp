#include "watever/ir.h"
#include "watever/opcode.h"
#include "watever/utils.h"
#include <algorithm>
#include <llvm/ADT/DenseMap.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/Support/Casting.h>
#include <memory>

using namespace watever;

llvm::DenseMap<llvm::Instruction *, int>
BlockLowering::getInternalUserCounts() {
  // TODO think about PHI nodes
  llvm::DenseMap<llvm::Instruction *, int> Result;

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

llvm::SmallVector<llvm::Instruction *> BlockLowering::getLiveOut() {
  llvm::SmallVector<llvm::Instruction *> Result;

  for (auto &Val : *BB) {
    if (Val.getNumUses() == 0) {
      continue;
    }
    if (Val.getOpcode() == llvm::Instruction::Store) {
      Result.push_back(&Val);
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

std::unique_ptr<WasmActions> BlockLowering::lower(Function &F) {
  // The last "live-out" value is handled first, ensuring that all live-out
  // values are emitted in the correct order - especially stores.
  auto ToHandle = getLiveOut();
  auto Count = getInternalUserCounts();

  if (auto *Br = llvm::dyn_cast<llvm::BranchInst>(BB->getTerminator())) {
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

    // TODO really 1?
    if (Count[Next] == 1) {
      // TODO only needed if we have more users
      // TODO map local to inst, so we can get it later
      auto Local = F.getNewLocal();
      Actions.Insts.push_back(WasmInst(Opcode::LocalTee, Local));
      // Adds the instruction to our actions
      visit(Next);
      for (llvm::Value *Op : Next->operands()) {
        if (auto *Inst = llvm::dyn_cast<llvm::Instruction>(Op)) {
          ToHandle.push_back(Inst);
        } else if (auto *Const = llvm::dyn_cast<llvm::ConstantInt>(Op)) {
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
        } else {
          WATEVER_TODO("put {} on top of the stack", llvmToString(*Op));
        }
      }
    } else {
      Count[Next]--;
      // TODO lookup local
      Actions.Insts.push_back(WasmInst(Opcode::LocalGet, 0));
    }
  }

  std::reverse(Actions.Insts.begin(), Actions.Insts.end());
  return std::make_unique<WasmActions>(Actions);
}

std::unique_ptr<Wasm> FunctionLowering::doBranch(llvm::BasicBlock *Source,
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
    llvm::SmallVector<llvm::BasicBlock *> MergeChildren, Context Ctx) {

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
            std::move(doBranch(Parent, Br->getSuccessor(0), Ctx)),
            std::move(doBranch(Parent, Br->getSuccessor(1), Ctx)));
      } else {
        assert(Br->getNumSuccessors() == 1 &&
               "expected only one successor in unconditional branch");

        WATEVER_LOG_TRACE("{} branches to {}", Parent->getName().str(),
                          Br->getSuccessor(0)->getName().str());

        Leaving = doBranch(Parent, Br->getSuccessor(0), Ctx);
      }
    } else if (auto *Ret = llvm::dyn_cast<llvm::ReturnInst>(Term)) {
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
int FunctionLowering::index(llvm::BasicBlock *BB, Context &Ctx) {
  int I = 0;
  for (auto &Syntax : Ctx) {
    if (Syntax.Label == BB) {
      return I;
    }
    ++I;
  }
  WATEVER_UNREACHABLE("unknown branch target");
}

std::unique_ptr<WasmActions>
FunctionLowering::translateBB(llvm::BasicBlock *BB) {
  BlockLowering BL{BB};
  return BL.lower(F);
}

void FunctionLowering::getMergeChildren(
    llvm::BasicBlock *R, llvm::SmallVectorImpl<llvm::BasicBlock *> &Result) {
  if (auto *Node = DT.getNode(R)) {
    for (auto *Child : *Node) {
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
  for (auto &F : Mod) {
    auto &DT = FAM.getResult<llvm::DominatorTreeAnalysis>(F);
    auto &LI = FAM.getResult<llvm::LoopAnalysis>(F);

    FunctionLowering FL{DT, LI};
    auto Wasm = FL.lower();
    WasmPrinter Printer{};
    Wasm->accept(Printer);
  }
  return Module{};
}
