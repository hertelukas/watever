#include "watever/binary.h"
#include "watever/ir.h"
#include <llvm/Analysis/CGSCCPassManager.h>
#include <llvm/Analysis/LoopAnalysisManager.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/SourceMgr.h>

#define ARGS_NOEXCEPT
#include "args/args.hxx"

#include <watever/legalization.h>
#include <watever/utils.h>

int main(int argc, char *argv[]) {
  args::ArgumentParser Parser("Watever");
  args::HelpFlag Help(Parser, "help", "Display help", {'h', "help"});

  args::ValueFlag<unsigned> LogLevel(
      Parser, "log_level",
      "Set the log level to 0=NONE, 1=ERR, 2=WARN(default), 3=INFO, 4=DEBUG, "
      ">5=TRACE",
      {'l', "log-level"}, 2);

  args::Positional<std::string> IRPath(
      Parser, "IRPath", "Path to the input IR file", args::Options::Required);

  args::ValueFlag<std::string> OutputPath(
      Parser, "output_path", "Path to the output file", {'o', "output"});

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
    return 1;
  }

  WATEVER_LOG_DBG("File name: {}", Mod->getSourceFileName());
  WATEVER_LOG_DBG("Bit width: {}", Mod->getDataLayout().getPointerSizeInBits());

  // Analysis Managers
  // See https://llvm.org/docs/NewPassManager.html
  llvm::LoopAnalysisManager LAM;
  llvm::FunctionAnalysisManager FAM;
  llvm::CGSCCAnalysisManager CGAM;
  llvm::ModuleAnalysisManager MAM;

  llvm::PassBuilder PB;
  PB.registerModuleAnalyses(MAM);
  PB.registerCGSCCAnalyses(CGAM);
  PB.registerFunctionAnalyses(FAM);
  PB.registerLoopAnalyses(LAM);
  PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

  llvm::ModulePassManager MPM;

  llvm::FunctionPassManager FPM;
  FPM.addPass(watever::LegalizationPass(*Mod));

  MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));

  MPM.run(*Mod, MAM);

  watever::ModuleLowering LoweringContext{};
  auto LoweredModule = LoweringContext.convert(*Mod, FAM);

  // Write to output file
  if (!OutputPath) {
    return 0;
  }

  std::string OutFile = args::get(OutputPath);
  std::error_code EC;
  llvm::raw_fd_ostream OS(OutFile, EC, llvm::sys::fs::OF_None);

  if (EC) {
    WATEVER_LOG_WARN("Error opening output file: {}", EC.message());
    return 1;
  }

  // Mod->print(OS, nullptr);

  watever::BinaryWriter Writer{OS, LoweredModule};
  Writer.write();
  return 0;
}
