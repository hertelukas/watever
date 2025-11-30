#include "watever/ir.h"
#include "watever/utils.h"
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/PassManager.h>

using namespace watever;

Wasm IRGenContext::doTree(llvm::BasicBlock *Root, Context &Ctx,
                          llvm::DominatorTree &DT) {
  WATEVER_UNIMPLEMENTED("doTree");
}

Module IRGenContext::convert(llvm::Module &Mod,
                             llvm::FunctionAnalysisManager &FAM) {
  for (auto &F : Mod) {
    Context Ctx;
    auto &DT = FAM.getResult<llvm::DominatorTreeAnalysis>(F);

    doTree(DT.getRoot(), Ctx, DT);
  }
  return Module{};
}
