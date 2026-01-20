#include "watever/FixFunctionBitcasts.hpp"
#include <llvm/Analysis/CGSCCPassManager.h>
#include <llvm/Analysis/LoopAnalysisManager.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Transforms/Scalar/ADCE.h>
#include <llvm/Transforms/Utils/BreakCriticalEdges.h>

#define ARGS_NOEXCEPT
#include "args/args.hxx"

#include "watever/binary.hpp"
#include "watever/ir.hpp"
#include "watever/legalization.hpp"
#include "watever/target.hpp"
#include "watever/utils.hpp"

struct FeatureOption {
  std::unique_ptr<args::Group> Group;
  std::unique_ptr<args::Flag> Enable;
  std::unique_ptr<args::Flag> Disable;
};

int main(int argc, char *argv[]) {
  args::ArgumentParser Parser("Watever");
  args::HelpFlag Help(Parser, "help", "Display help", {'h', "help"});

  args::ValueFlag<unsigned> LogLevel(
      Parser, "log_level",
      "Set the log level to 0=NONE, 1=ERR, 2=WARN(default), 3=INFO, 4=DEBUG, "
      ">5=TRACE",
      {'l', "log-level"}, 2);

  args::ValueFlag<std::string> OutputPath(
      Parser, "output_path", "Path to the output file", {'o', "output"});

  args::Flag LegalOnly(Parser, "legal", "Run only legalization and print IR",
                       {"legal"});

  args::Flag LegalAndOpt(Parser, "legal_opt",
                         "Run legalization and LLVM passes", {"legal_opt"});

  args::Flag DisableColoring(Parser, "Disable Coloring Pass",
                             "Skip coloring pass and assign locals greedily",
                             {"disable-coloring"}, false);

  args::Group FeatureGroup(
      Parser, "WebAssembly Features (--disable-<feature> to disable)");
  std::unordered_map<std::string, FeatureOption> Features;

  // Feature flags
#define WATEVER_FEATURE(VAR, NAME, DEFAULT, HELP)                              \
  {                                                                            \
    auto &Opt = Features[#VAR];                                                \
    std::string Status = DEFAULT ? "(default ON)" : " (default OFF)";          \
    Opt.Group = std::make_unique<args::Group>(                                 \
        FeatureGroup, "", args::Group::Validators::AtMostOne);                 \
    Opt.Enable = std::make_unique<args::Flag>(                                 \
        *Opt.Group, NAME, std::string("Enable " HELP " ") + Status,            \
        args::Matcher{NAME});                                                  \
    Opt.Disable = std::make_unique<args::Flag>(                                \
        *Opt.Group, "disable-" NAME, "Disable " HELP,                          \
        args::Matcher{"disable-" NAME}, args::Options::Hidden);                \
  }

#include "watever/feature.def"
#undef WATEVER_FEATURE

  args::Positional<std::string> IRPath(
      Parser, "IRPath", "Path to the input IR file", args::Options::Required);

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
  watever::TargetConfig Config{};
  Config.DoColoring = !DisableColoring.Get();

#define WATEVER_FEATURE(VAR, NAME, DEFAULT, HELP)                              \
  {                                                                            \
    bool IsEnabled = DEFAULT;                                                  \
    auto &Opt = Features[#VAR];                                                \
    if (*Opt.Enable) {                                                         \
      IsEnabled = true;                                                        \
    }                                                                          \
    if (*Opt.Disable) {                                                        \
      IsEnabled = false;                                                       \
    }                                                                          \
    Config.EnabledFeatures.set_##VAR##_enabled(IsEnabled);                     \
  }
#include "watever/feature.def"
#undef WATEVER_FEATURE

  // Setup output
  llvm::raw_ostream *OS = &llvm::outs();
  std::unique_ptr<llvm::raw_fd_ostream> FileOS;

  if (OutputPath) {
    std::string OutFile = args::get(OutputPath);
    std::error_code EC;

    FileOS = std::make_unique<llvm::raw_fd_ostream>(OutFile, EC,
                                                    llvm::sys::fs::OF_None);

    if (EC) {
      WATEVER_LOG_WARN("Error opening output file: {}", EC.message());
      return 1;
    }
    OS = FileOS.get();
  }

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

  llvm::ModulePassManager LegalizeMPM;

  LegalizeMPM.addPass(watever::FixFunctionBitcastsPass());
  LegalizeMPM.addPass(watever::LegalizationPass(Config));

  LegalizeMPM.run(*Mod, MAM);

#ifdef WATEVER_LOGGING
  if (llvm::verifyModule(*Mod, &llvm::errs())) {
    WATEVER_LOG_ERR("Legalization left the module in an inconsistent state!");
    return 1;
  }
  WATEVER_LOG_INFO("Legalization returned a legal module.");
#endif
  if (LegalOnly) {
    Mod->print(*OS, nullptr);
    return 0;
  }

  llvm::ModulePassManager OptimizePM;
  llvm::FunctionPassManager FPM;
  FPM.addPass(llvm::ADCEPass());
  FPM.addPass(llvm::BreakCriticalEdgesPass());

  OptimizePM.addPass(llvm::createModuleToFunctionPassAdaptor(std::move(FPM)));
  OptimizePM.run(*Mod, MAM);

  if (LegalAndOpt) {
    Mod->print(*OS, nullptr);
    return 0;
  }

  watever::ModuleLowering LoweringContext{};
  auto LoweredModule = LoweringContext.convert(*Mod, FAM, Config);

  watever::BinaryWriter Writer{*OS, LoweredModule};
  Writer.write();
  return 0;
}
