#pragma once

#include "watever/linking.hpp"
#include "watever/type.hpp"
#include <cstdint>
#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/SmallVector.h>

namespace watever {

class Wasm;
class WasmVisitor;

struct Symbol {
  uint32_t SymbolIndex{};
  uint32_t LinkerFlags{};

  explicit Symbol(uint32_t SI) : SymbolIndex(SI) {}
  void setFlag(SymbolFlag Flag) { LinkerFlags |= static_cast<uint32_t>(Flag); }
  bool isSet(SymbolFlag Flag) {
    return (LinkerFlags & static_cast<uint32_t>(Flag)) != 0;
  }

  virtual ~Symbol() = default;
};

struct Global : public Symbol {
  uint32_t GlobalIdx;
  ValType Type;
  bool Mutable;
  explicit Global(uint32_t SymbolIdx, uint32_t GlobalIdx, ValType Ty, bool Mut)
      : Symbol(SymbolIdx), GlobalIdx(GlobalIdx), Type(Ty), Mutable(Mut) {}
};

struct ImportedGlobal final : public Global {
  std::string ModuleName;
  std::string ItemName;
  explicit ImportedGlobal(uint32_t SymbolIdx, uint32_t GlobalIdx, ValType Ty,
                          bool Mut, std::string MN, std::string IN)
      : Global(SymbolIdx, GlobalIdx, Ty, Mut), ModuleName(std::move(MN)),
        ItemName(std::move(IN)) {}
};

struct DefinedGlobal final : public Global {
  std::unique_ptr<Wasm> Expr;
  explicit DefinedGlobal(uint32_t SymbolIdx, uint32_t GlobalIdx, ValType Ty,
                         bool Mut, std::unique_ptr<Wasm> Ex);
};

struct Function : public Symbol {
  std::string Name;
  uint32_t TypeIndex;
  uint32_t FunctionIndex;

  Function(uint32_t SymbolIdx, uint32_t TypeIdx, uint32_t FuncIdx,
           std::string Nm)
      : Symbol(SymbolIdx), Name(std::move(Nm)), TypeIndex(TypeIdx),
        FunctionIndex(FuncIdx) {}

  // Needed to decide if we have to write out the SymbolName in the relocation
  virtual bool isImport() = 0;

  ~Function() override = default;
};

struct ImportedFunc final : public Function {
  explicit ImportedFunc(uint32_t SymbolIdx, uint32_t TypeIdx, uint32_t FuncIdx,
                        llvm::StringRef Name)
      : Function(SymbolIdx, TypeIdx, FuncIdx, Name.str()) {}

  bool isImport() override { return true; }
};

struct Local {
  uint32_t Index;
};

class DefinedFunc final : public Function {
  friend class FunctionLowering;
  uint32_t TotalLocals{};

public:
  uint32_t Args{};
  std::unique_ptr<Wasm> Body{};
  llvm::DenseMap<llvm::Value *, Local *> LocalMapping;
  llvm::DenseMap<ValType, llvm::SmallVector<std::unique_ptr<Local>>> Locals{};

  Local *SavedSP{};
  explicit DefinedFunc(uint32_t SymbolIdx, uint32_t TypeIdx, uint32_t FuncIdx,
                       uint32_t Args, llvm::StringRef Name);

  Local *getNewLocal(ValType Ty) {
    auto NewLocal = std::make_unique<Local>(TotalLocals++);
    auto *Result = NewLocal.get();
    Locals[Ty].push_back(std::move(NewLocal));
    return Result;
  }

  bool isImport() override { return false; }
  void visit(WasmVisitor &V) const;
};

} // namespace watever
