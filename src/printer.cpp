#include "watever/printer.hpp"
#include "watever/opcode.hpp"
#include "watever/utils.hpp"
#include <llvm/Support/raw_ostream.h>

namespace watever {

#ifdef WATEVER_LOGGING
void dumpWasm(WasmActions &Body) {
  std::string Buffer;
  llvm::raw_string_ostream OS(Buffer);

  size_t Depth = 0;

  for (const auto &Inst : Body.Insts) {
    if (Inst.Op == Opcode::End || Inst.Op == Opcode::Else) {
      Depth--;
    }

    for (size_t I = 0; I < Depth; ++I) {
      OS << "|  ";
    }

    OS << Inst.getString() << "\n";

    if (Inst.Op == Opcode::Block || Inst.Op == Opcode::Loop ||
        Inst.Op == Opcode::If || Inst.Op == Opcode::Else) {
      Depth++;
    }
  }

  WATEVER_LOG_DBG("\n{}", OS.str());
}
#endif
} // namespace watever
