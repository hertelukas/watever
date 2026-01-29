#pragma once

#include "watever/feature.hpp"
#include "watever/linking.hpp"
#include "watever/type.hpp"
#include <cstdint>
#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/SmallPtrSet.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>

namespace watever {

class Wasm;
class WasmVisitor;

struct Symbol {
  enum class Kind : uint8_t {
    ImportedGlobal,
    DefinedGlobal,
    ImportedFunc,
    DefinedFunc,
    AliasedFunc,
    UndefinedData,
    DefinedData,
    UndefinedTable,
    DefinedTable,
  };

  const Kind ClassKind;
  uint32_t SymbolIndex{};
  uint32_t LinkerFlags{};

  explicit Symbol(Kind K, uint32_t SI) : ClassKind(K), SymbolIndex(SI) {}
  [[nodiscard]] Kind getClassKind() const { return ClassKind; }

  void setFlag(SymbolFlag Flag) { LinkerFlags |= static_cast<uint32_t>(Flag); }
  bool isSet(SymbolFlag Flag) {
    return (LinkerFlags & static_cast<uint32_t>(Flag)) != 0;
  }

  void setFlags(llvm::GlobalValue &GV) {
    if (GV.hasLocalLinkage()) {
      setFlag(SymbolFlag::WASM_SYM_BINDING_LOCAL);
    }
    if (GV.hasWeakLinkage() || GV.hasLinkOnceLinkage()) {
      setFlag(SymbolFlag::WASM_SYM_BINDING_WEAK);
    }
    if (GV.hasHiddenVisibility()) {
      setFlag(SymbolFlag::WASM_SYM_VISIBILITY_HIDDEN);
    }
  }

  [[nodiscard]] virtual SymbolKind getKind() const = 0;
  virtual ~Symbol() = default;
};

struct Global : public Symbol {
  uint32_t GlobalIdx;
  ValType Type;
  bool Mutable;
  explicit Global(Kind K, uint32_t SymbolIdx, uint32_t GlobalIdx, ValType Ty,
                  bool Mut)
      : Symbol(K, SymbolIdx), GlobalIdx(GlobalIdx), Type(Ty), Mutable(Mut) {}

  [[nodiscard]] SymbolKind getKind() const override {
    return SymbolKind::SYMTAB_GLOBAL;
  }

  static bool classof(const Symbol *S) {
    return S->getClassKind() == Kind::ImportedGlobal ||
           S->getClassKind() == Kind::DefinedGlobal;
  }
};

struct ImportedGlobal final : public Global {
  std::string ModuleName;
  std::string ItemName;
  explicit ImportedGlobal(uint32_t SymbolIdx, uint32_t GlobalIdx, ValType Ty,
                          bool Mut, std::string MN, std::string IN)
      : Global(Kind::ImportedGlobal, SymbolIdx, GlobalIdx, Ty, Mut),
        ModuleName(std::move(MN)), ItemName(std::move(IN)) {}
  static bool classof(const Symbol *S) {
    return S->getClassKind() == Kind::ImportedGlobal;
  }
};

struct DefinedGlobal final : public Global {
  std::unique_ptr<Wasm> Expr;
  explicit DefinedGlobal(uint32_t SymbolIdx, uint32_t GlobalIdx, ValType Ty,
                         bool Mut, std::unique_ptr<Wasm> Ex);
  static bool classof(const Symbol *S) {
    return S->getClassKind() == Kind::DefinedGlobal;
  }
};

struct Function : public Symbol {
  std::string Name;
  uint32_t TypeIndex;
  uint32_t FunctionIndex;

  Function(Kind K, uint32_t SymbolIdx, uint32_t TypeIdx, uint32_t FuncIdx,
           std::string Nm)
      : Symbol(K, SymbolIdx), Name(std::move(Nm)), TypeIndex(TypeIdx),
        FunctionIndex(FuncIdx) {}

  // Needed to decide if we have to write out the SymbolName in the relocation
  virtual bool isImport() = 0;
  [[nodiscard]] SymbolKind getKind() const override {
    return SymbolKind::SYMTAB_FUNCTION;
  }

  static bool classof(const Symbol *S) {
    return S->getClassKind() == Kind::ImportedFunc ||
           S->getClassKind() == Kind::DefinedFunc ||
           S->getClassKind() == Kind::AliasedFunc;
  }

  ~Function() override = default;
};

struct ImportedFunc final : public Function {
  explicit ImportedFunc(uint32_t SymbolIdx, uint32_t TypeIdx, uint32_t FuncIdx,
                        llvm::StringRef Name)
      : Function(Kind::ImportedFunc, SymbolIdx, TypeIdx, FuncIdx, Name.str()) {}

  bool isImport() override { return true; }

  static bool classof(const Symbol *S) {
    return S->getClassKind() == Kind::ImportedFunc;
  }
};

class DefinedFunc final : public Function {
  friend class FunctionLowering;
  // This is only used for debugging, to assign inideces to locals. The final
  // index has to be ordered by local type, so cannot be arbitrary and will get
  // replaced when writing out to the binary
  uint32_t TotalLocals{};

public:
  uint32_t TotalArgs{};
  Features FeatureSet;
  std::unique_ptr<Wasm> Body{};
  llvm::DenseMap<llvm::Value *, uint32_t> LocalMapping;
  // All locals, consisting of arguments and extra locals
  llvm::SmallVector<uint32_t> AllLocals{};
  llvm::DenseMap<ValType, llvm::SmallVector<uint32_t>> Locals{};
  llvm::DenseMap<ValType, llvm::SmallVector<uint32_t>> Arguments{};
  llvm::DenseMap<llvm::Instruction *, uint32_t> StackSlots{};

  llvm::DenseSet<llvm::AllocaInst *> PromotedAllocas{};
  llvm::DenseMap<llvm::BasicBlock *, llvm::SmallVector<llvm::Instruction *>> Roots;

  std::optional<uint32_t> FP{};
  int64_t FrameSize{};
  explicit DefinedFunc(uint32_t SymbolIdx, uint32_t TypeIdx, uint32_t FuncIdx,
                       llvm::Function *F, Features Feat);

  uint32_t getNewLocal(ValType Ty) {
    auto NewLocal = TotalLocals++;
    AllLocals.push_back(NewLocal);
    Locals[Ty].push_back(NewLocal);
    return NewLocal;
  }

  uint32_t getOrCreateLocal(llvm::Value *V, const llvm::DataLayout &DL) {
    if (LocalMapping.contains(V)) {
      return LocalMapping.lookup(V);
    }
    WATEVER_LOG_INFO("{} did not have a local assigned to it, creating...",
                     V->getNameOrAsOperand());

    auto Ty = fromLLVMType(V->getType(), DL);
    uint32_t Local = getNewLocal(Ty);
    LocalMapping[V] = Local;
    return Local;
  }

  bool isImport() override { return false; }
  void visit(WasmVisitor &V) const;
  static bool classof(const Symbol *S) {
    return S->getClassKind() == Kind::DefinedFunc;
  }

  /// Check whether \p Val has a user outside of \p BB
  bool hasExternalUser(llvm::Value *Val, llvm::BasicBlock *BB);
  void setupStackFrame(llvm::BasicBlock *Entry);
};

class AliasedFunc final : public Function {

public:
  explicit AliasedFunc(uint32_t SymbolIdx, std::string Name, Function *Aliasee)
      : Function(Kind::AliasedFunc, SymbolIdx, Aliasee->TypeIndex,
                 Aliasee->FunctionIndex, std::move(Name)) {}

  bool isImport() override { return false; }
  static bool classof(const Symbol *S) {
    return S->getClassKind() == Kind::AliasedFunc;
  }
};

struct Data : public Symbol {
  std::string Name;
  explicit Data(Kind K, uint32_t SymbolIdx, std::string Name)
      : Symbol(K, SymbolIdx), Name(std::move(Name)) {}

  [[nodiscard]] SymbolKind getKind() const override {
    return SymbolKind::SYMTAB_DATA;
  }

  static bool classof(const Symbol *S) {
    return S->getClassKind() == Kind::DefinedData ||
           S->getClassKind() == Kind::UndefinedData;
  }
};

// Data that appears in the data section of the WebAssembly binary
struct DefinedData final : public Data {
  uint32_t DataIndex;
  bool Active;
  std::vector<uint8_t> Content;
  llvm::SmallVector<std::unique_ptr<RelocationEntry>> Relocations;
  uint32_t Flags{};
  uint32_t Alignment{};
  DataSection Sec;

  explicit DefinedData(
      uint32_t SymbolIdx, uint32_t DataIdx, bool Active, std::string Name,
      llvm::ArrayRef<uint8_t> Content,
      llvm::SmallVector<std::unique_ptr<RelocationEntry>> Relocs, DataSection S)
      : Data(Kind::DefinedData, SymbolIdx, std::move(Name)), DataIndex(DataIdx),
        Active(Active), Content(Content.begin(), Content.end()),
        Relocations(std::move(Relocs)), Sec(S) {}

  void setSegmentFlag(SegmentFlag F) { Flags |= static_cast<uint32_t>(F); }

  static bool classof(const Symbol *S) {
    return S->getClassKind() == Kind::DefinedData;
  }
};

// Undefined data, which does neither appear in the data section, nor has a
// segment info
struct UndefinedData final : public Data {
  explicit UndefinedData(uint32_t SymbolIdx, std::string Name)
      : Data(Kind::UndefinedData, SymbolIdx, std::move(Name)) {}

  static bool classof(const Symbol *S) {
    return S->getClassKind() == Kind::UndefinedData;
  }
};

struct Table : public Symbol {
  ValType Type;
  uint32_t TableIndex;
  explicit Table(Kind K, uint32_t SymbolIdx, uint32_t TableIdx, ValType ET)
      : Symbol(K, SymbolIdx), Type(ET), TableIndex(TableIdx) {}
  [[nodiscard]] SymbolKind getKind() const override {
    return SymbolKind::SYMTAB_TABLE;
  }

  static bool classof(const Symbol *S) {
    return S->getClassKind() == Kind::DefinedTable ||
           S->getClassKind() == Kind::UndefinedTable;
  }
};

struct DefinedTable final : public Table {
  explicit DefinedTable(uint32_t SymbolIdx, uint32_t TableIdx,
                        ValType ElementType)
      : Table(Kind::DefinedTable, SymbolIdx, TableIdx, ElementType) {}
  static bool classof(const Symbol *S) {
    return S->getClassKind() == Kind::DefinedTable;
  }
};

struct UndefinedTable final : public Table {
  std::string ItemName;
  explicit UndefinedTable(uint32_t SymbolIdx, uint32_t TableIdx,
                          ValType ElementType, std::string IN)
      : Table(Kind::UndefinedTable, SymbolIdx, TableIdx, ElementType),
        ItemName(std::move(IN)) {}
  static bool classof(const Symbol *S) {
    return S->getClassKind() == Kind::UndefinedTable;
  }
};
} // namespace watever
