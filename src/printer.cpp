#include "watever/printer.hpp"

namespace watever {
#ifdef WATEVER_LOGGING
class WasmPrinter final : public WasmVisitor {
  unsigned int Depth = 0;

  void print(llvm::StringRef Text) const {
    std::string Indent;
    for (size_t I = 0; I < Depth; ++I) {
      Indent += "|  ";
    }

    WATEVER_LOG_DBG("{}{}", Indent, Text.str());
  }

public:
  void visit(WasmBlock &Block) override {
    print("block");
    Depth++;
    Block.InnerWasm->accept(*this);
    print("end_block");
    Depth--;
  }

  void visit(WasmLoop &Loop) override {
    print("loop");
    Depth++;
    Loop.InnerWasm->accept(*this);
    print("end_loop");
    Depth--;
  }

  void visit(WasmIf &IfElse) override {
    print("if");
    Depth++;
    IfElse.True->accept(*this);
    Depth--;
    print("else");
    Depth++;
    IfElse.False->accept(*this);
    print("end_if");
    Depth--;
  }

  void visit(WasmReturn &) override { print("ret"); }

  void visit(WasmSeq &Seq) override {
    Seq.Flow.first->accept(*this);
    Seq.Flow.second->accept(*this);
  }

  void visit(WasmActions &Actions) override {
    for (const auto &Inst : Actions.Insts) {
      print(Inst.getString());
    }
  }

  void visit(WasmBr &Br) override { print("br " + std::to_string(Br.Nesting)); }
};
#endif

#ifdef WATEVER_LOGGING
void dumpWasm(Wasm &W) {
  WasmPrinter Printer{};
  W.accept(Printer);
}
#endif
} // namespace watever
