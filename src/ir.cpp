#include "watever/ir.h"
#include "watever/utils.h"

using namespace watever;

std::unique_ptr<Wasm> FunctionLowering::doTree(llvm::BasicBlock *Root,
                                               Context &Ctx) {
  WATEVER_LOG_TRACE("doTree with root {}", Root->getName().str());

  llvm::SmallVector<llvm::BasicBlock *> MergeChildren;

  // Emit loop block
  if (LI.isLoopHeader(Root)) {
    WATEVER_LOG_TRACE("Generating loop for {}", Root->getName().str());
    Ctx.push_back(ContainingSyntax::createLoop(Root));
    return std::make_unique<Wasm>(
        WasmLoop{nodeWithin(Root, MergeChildren, Ctx)});
  }

  return nodeWithin(Root, MergeChildren, Ctx);
}

std::unique_ptr<Wasm> FunctionLowering::nodeWithin(
    llvm::BasicBlock *Parent,
    llvm::SmallVector<llvm::BasicBlock *> MergeChildren, Context &Ctx) {
  WATEVER_UNIMPLEMENTED("nodeWithin");
}

void FunctionLowering::getMergeChildren(
    llvm::BasicBlock *R, llvm::SmallVectorImpl<llvm::BasicBlock *> &Result) {
  if (auto *Node = DT.getNode(R)) {
    for (auto *Child : *Node) {
      Result.push_back(Node->getBlock());
    }
  }
}

Module ModuleLowering::convert(llvm::Module &Mod,
                               llvm::FunctionAnalysisManager &FAM) {
  for (auto &F : Mod) {
    auto &DT = FAM.getResult<llvm::DominatorTreeAnalysis>(F);
    auto &LI = FAM.getResult<llvm::LoopAnalysis>(F);

    FunctionLowering FL{DT, LI};
    FL.lower();
  }
  return Module{};
}
