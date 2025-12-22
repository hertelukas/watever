#include "watever/symbol.hpp"
#include "watever/ir.hpp"

using namespace watever;

DefinedGlobal::DefinedGlobal(uint32_t SymbolIdx, uint32_t GlobalIdx, ValType Ty,
                             bool Mut, std::unique_ptr<Wasm> Ex)
    : Global(Kind::DefinedGlobal, SymbolIdx, GlobalIdx, Ty, Mut),
      Expr(std::move(Ex)) {}

DefinedFunc::DefinedFunc(uint32_t SymbolIdx, uint32_t TypeIdx, uint32_t FuncIdx,
                         uint32_t Args, llvm::StringRef Name)
    : Function(Kind::DefinedFunc, SymbolIdx, TypeIdx, FuncIdx, Name.str()),
      Args(Args) {}

void DefinedFunc::visit(WasmVisitor &V) const { Body->accept(V); }
