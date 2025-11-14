#ifndef LEGALIZATION_H
#define LEGALIZATION_H

#include <llvm/IR/Analysis.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/PassManager.h>

namespace watever {

class LegalizationPass : public llvm::PassInfoMixin<LegalizationPass> {
public:
  llvm::PreservedAnalyses run(llvm::Function &F,
                              llvm::FunctionAnalysisManager &AM);
};
} // namespace watever

#endif /* LEGALIZATION_H */
