#pragma once
#include <llvm/IR/Analysis.h>
#include <llvm/IR/PassManager.h>

namespace watever {
struct AddMissingPrototypes
    : public llvm::PassInfoMixin<AddMissingPrototypes> {
  llvm::PreservedAnalyses run(llvm::Module &M, llvm::ModuleAnalysisManager &AM);
};
} // namespace watever
