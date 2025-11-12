#include "llvm/IR/CFG.h"
#include <llvm/ADT/PostOrderIterator.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/Support/ReverseIteration.h>
#include <llvm/Support/SourceMgr.h>
#include <memory>
#define ARGS_NOEXCEPT
#include "args/args.hxx"

#include <watever/graph-builder.h>
#include <watever/utils.h>

int main(int argc, char *argv[]) {
  args::ArgumentParser Parser("Watever");
  args::HelpFlag Help(Parser, "help", "Display help", {'h', "help"});

  args::ValueFlag<unsigned> LogLevel(
      Parser, "log_level",
      "Set the log level to 0=NONE, 1=ERR, 2=WARN(default), 3=INFO, 4=DEBUG, "
      ">5=TRACE",
      {'l', "log-level"}, 2);

  args::Positional<std::string> IRPath(Parser, "IRPath",
                                       "Path to the input IR file", "-");

  Parser.ParseCLI(argc, argv);
  if (Parser.GetError() == args::Error::Help) {
    std::cout << Parser;
    return 0;
  }
  if (Parser.GetError() != args::Error::None) {
    std::cerr << "Error parsing arguments: " << Parser.GetErrorMsg() << '\n';
    return 1;
  }

#ifdef WATEVER_LOGGING
  {
    spdlog::level::level_enum Level = spdlog::level::off;
    switch (LogLevel.Get()) {
    case 0:
      Level = spdlog::level::off;
      break;
    case 1:
      Level = spdlog::level::err;
      break;
    case 2:
      Level = spdlog::level::warn;
      break;
    case 3:
      Level = spdlog::level::info;
      break;
    case 4:
      Level = spdlog::level::debug;
      break;
    default:
      assert(Level >= 5);
      Level = spdlog::level::trace;
      break;
    }

    spdlog::set_level(Level);
  }
#endif

  auto Context = std::make_unique<llvm::LLVMContext>();
  llvm::SMDiagnostic Diag{};
  auto Mod = llvm::parseIRFile(IRPath.Get(), Diag, *Context);
  if (!Mod) {
    Diag.print(argv[0], llvm::errs());
  }

  WATEVER_LOG_DBG("File name: {}", Mod->getSourceFileName());
  WATEVER_LOG_DBG("Bit width: {}", Mod->getDataLayout().getPointerSizeInBits());

  auto GB = std::make_unique<watever::GraphBuilder>();

  for (const auto &F : Mod->getFunctionList()) {
    WATEVER_LOG_DBG("Handling function: {}", F.getName().str());
    llvm::ReversePostOrderTraversal<const llvm::Function *> PROT(&F);
    for (const llvm::BasicBlock *LLVMBB : PROT) {
      llvm::BasicBlock::const_iterator const Begin = LLVMBB->getFirstNonPHIIt();
      llvm::BasicBlock::const_iterator const End = LLVMBB->end();

      // TODO care about tail calls. This is inspired by
      // SelectionDAGIsel.cpp:856
      for (llvm::BasicBlock::const_iterator I = Begin; I != End; ++I) {
        GB->visit(*I);
      }
      GB->clear();
    }
    // TODO SelectionDAGIsel.cpp:856
  }

  return 0;
}
