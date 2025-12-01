#include "watever/ir.h"
#include "watever/utils.h"
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <memory>

using namespace watever;

std::unique_ptr<Wasm> FunctionLowering::doBranch(llvm::BasicBlock *Source,
                                                 llvm::BasicBlock *Target,
                                                 Context Ctx) {

  // Backward branch (continue)
  if (DT.dominates(Target, Source)) {
    WATEVER_LOG_TRACE("backwards branch from {} to {}", Source->getName().str(),
                      Target->getName().str());
    WATEVER_TODO("calcualte depth, and pass (br always needs i, delete default "
                 "constructor)");
    // TODO merge with forward
    return std::make_unique<WasmBr>(WasmBr{});
  }

  // Forward branch (exit)
  if (isMergeNode(Target)) {
    WATEVER_LOG_TRACE("forwards branch from {} to {}", Source->getName().str(),
                      Target->getName().str());

    return std::make_unique<WasmBr>(WasmBr{});
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

std::unique_ptr<WasmActions>
FunctionLowering::translateBB(llvm::BasicBlock *BB) {
  WATEVER_TODO("translate {}", BB->getName().str());
  return std::make_unique<WasmActions>();
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
