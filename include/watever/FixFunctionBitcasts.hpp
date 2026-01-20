#pragma once
#include <llvm/IR/Analysis.h>
#include <llvm/IR/PassManager.h>

namespace watever {
struct FixFunctionBitcastsPass
    : public llvm::PassInfoMixin<FixFunctionBitcastsPass> {
  llvm::PreservedAnalyses run(llvm::Module &M, llvm::ModuleAnalysisManager &AM);
};
} // namespace watever
