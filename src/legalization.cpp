#include "watever/utils.h"
#include <llvm/IR/Analysis.h>
#include <llvm/IR/PassManager.h>
#include <watever/legalization.h>

llvm::PreservedAnalyses
watever::LegalizationPass::run(llvm::Function &F,
                               llvm::FunctionAnalysisManager &AM) {
  WATEVER_LOG_DBG("Legalizing {}", F.getName().str());
  return llvm::PreservedAnalyses::all();
}
