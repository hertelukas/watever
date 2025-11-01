#include "wasgen/utils.h"
#include "llvm/Support/raw_ostream.h"
#include <llvm/IR/Module.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/Support/SourceMgr.h>

int main(int argc, char *argv[]) {
  llvm::SMDiagnostic Err;
  llvm::LLVMContext Ctx;

  auto Mod = llvm::parseIRFile(argv[1], Err, Ctx);

  if (!Mod) {
    Err.print(argv[0], llvm::errs());
    return 1;
  }

  llvm::outs() << "Instruction Count: " << Mod->getInstructionCount() << "\n";
  llvm::outs() << "File name: " << Mod->getSourceFileName() << "\n";
  return 0;
}
