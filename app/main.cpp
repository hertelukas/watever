#include "watever/binary.h"
#include "watever/structure.h"
#include <llvm/IR/Module.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/Support/ErrorHandling.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/raw_ostream.h>
#include <watever/opcode.h>

using namespace watever;
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
  llvm::outs() << "Functions:\n";
  for (auto const &F : Mod->getFunctionList()) {
    llvm::outs() << "------------------------\n";
    llvm::outs() << F;
  }

  Opcode Call = Opcode{Opcode::Enum::BrIf};
  llvm::outs() << Call.getCode() << ": " << Call.getName() << "\n";

  auto Encoded = llvm::SmallVector<std::byte>();
  leb128(-1234123412341234, Encoded);

  for (const auto &Byte : Encoded) {
    llvm::outs() << llvm::format("%02x", (unsigned int)Byte);
  }
  llvm::outs() << "\n";

  std::error_code EC;
  llvm::raw_fd_ostream FileOS(argv[2], EC);

  BinaryWriter Writer{FileOS};
  Module Module{};
  Writer.write(Module);
  return 0;
}
