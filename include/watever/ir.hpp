#pragma once

#include "watever/linking.hpp"
#include "watever/opcode.hpp"
#include "watever/symbol.hpp"
#include "watever/type.hpp"
#include "watever/utils.hpp"
#include <cstdint>
#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/iterator_range.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/IR/Analysis.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/InstVisitor.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/FormatVariadic.h>
#include <llvm/Support/LEB128.h>
#include <llvm/Support/raw_ostream.h>
#include <map>
#include <memory>
#include <string>
#include <variant>

namespace watever {
class Wasm;
class WasmBlock;
class WasmLoop;
class WasmIf;
class WasmBr;
class WasmReturn;
class WasmActions;
class WasmSeq;

class WasmVisitor {
public:
  virtual ~WasmVisitor() = default;

  virtual void visit(Wasm &) {};
  virtual void visit(WasmBlock &) = 0;
  virtual void visit(WasmLoop &) = 0;
  virtual void visit(WasmIf &) = 0;
  virtual void visit(WasmBr &) = 0;
  virtual void visit(WasmReturn &) = 0;
  virtual void visit(WasmActions &) = 0;
  virtual void visit(WasmSeq &) = 0;
};

class InstArgument {
public:
  virtual void encode(llvm::raw_ostream &) const = 0;
  virtual void addRelocation(llvm::raw_ostream &, Relocation &) {};
  [[nodiscard]] virtual std::string getString() const = 0;
  virtual ~InstArgument() = default;
};

class MemArg final : public InstArgument {
  uint32_t Alignment{};
  uint32_t MemIdx{};
  uint64_t Offset{};

public:
  MemArg() = default;
  MemArg(uint32_t Alignment, uint64_t Offset)
      : Alignment(Alignment), Offset(Offset) {}

  [[nodiscard]] std::string getString() const override {
    return llvm::formatv("align: {}, idx: {}, offset: {}", Alignment, MemIdx,
                         Offset);
  }

  void encode(llvm::raw_ostream &OS) const override {
    if (Alignment >= 64) {
      WATEVER_LOG_ERR("Alignment is too large: {}", Alignment);
    }
    uint32_t Flag = Alignment;

    if (MemIdx != 0) {
      Flag |= 64; // Signal that we have a MemIdx
    }
    llvm::encodeULEB128(Flag, OS);
    if (MemIdx != 0) {
      llvm::encodeULEB128(MemIdx, OS);
    }
    llvm::encodeULEB128(Offset, OS);
  }
};

class LocalArg final : public InstArgument {
public:
  Local *Lo;
  explicit LocalArg(Local *L) : Lo(L) {}
  [[nodiscard]] std::string getString() const override {
    return llvm::formatv("{}", Lo->Index);
  }
  void encode(llvm::raw_ostream &OS) const override {
    llvm::encodeULEB128(Lo->Index, OS);
  }
};

class RelocatableFuncArg final : public InstArgument {
public:
  Function *Func;
  explicit RelocatableFuncArg(Function *F) : Func(F) {}
  [[nodiscard]] std::string getString() const override {
    return llvm::formatv("{}", Func->FunctionIndex);
  }
  void encode(llvm::raw_ostream &OS) const override {
    // Must be padded to 5 bytes, so it can be patched by the linker
    llvm::encodeULEB128(Func->FunctionIndex, OS, 5);
  }

  void addRelocation(llvm::raw_ostream &OS, Relocation &Reloc) override {
    Reloc.Entries.emplace_back(RelocationType::R_WASM_FUNCTION_INDEX_LEB,
                               OS.tell(), Func->SymbolIndex);
  }
};

class RelocatableGlobalArg final : public InstArgument {
  Global *Gl;

public:
  explicit RelocatableGlobalArg(Global *G) : Gl(G) {}
  [[nodiscard]] std::string getString() const override {
    return llvm::formatv("{}", Gl->GlobalIdx);
  }
  void encode(llvm::raw_ostream &OS) const override {
    llvm::encodeULEB128(Gl->GlobalIdx, OS, 5);
  }

  void addRelocation(llvm::raw_ostream &OS, Relocation &Reloc) override {
    Reloc.Entries.emplace_back(RelocationType::R_WASM_GLOBAL_INDEX_LEB,
                               OS.tell(), Gl->SymbolIndex);
  }
};

class RelocatablePointerArg final : public InstArgument {
  Data *DT;

public:
  explicit RelocatablePointerArg(Data *D) : DT(D) {}
  [[nodiscard]] std::string getString() const override {
    return llvm::formatv("{}", DT->Name);
  }
  void encode(llvm::raw_ostream &OS) const override {
    llvm::encodeSLEB128(0, OS, 5);
  }

  void addRelocation(llvm::raw_ostream &OS, Relocation &Reloc) override {
    Reloc.Entries.emplace_back(RelocationType::R_WASM_MEMORY_ADDR_SLEB,
                               OS.tell(), DT->SymbolIndex);
  }
};

class RelocatableIndirectCallArg final : public InstArgument {
  uint32_t TypeIndex;
  Table *Tab;

public:
  explicit RelocatableIndirectCallArg(uint32_t TypeIdx,
                                      Table *IndirectFunctionTable)
      : TypeIndex(TypeIdx), Tab(IndirectFunctionTable) {}

  [[nodiscard]] std::string getString() const override {
    return llvm::formatv("{} {}", TypeIndex, Tab->TableIndex).str();
  }
  void encode(llvm::raw_ostream &OS) const override {
    llvm::encodeULEB128(TypeIndex, OS, 5);
    llvm::encodeULEB128(Tab->TableIndex, OS, 5);
  }

  void addRelocation(llvm::raw_ostream &OS, Relocation &Reloc) override {
    Reloc.Entries.emplace_back(RelocationType::R_WASM_TYPE_INDEX_LEB, OS.tell(),
                               TypeIndex);
    Reloc.Entries.emplace_back(RelocationType::R_WASM_TABLE_NUMBER_LEB,
                               OS.tell() + 5, Tab->SymbolIndex);
  }
};

class RelocatableTableIndexArg final : public InstArgument {
  Function *F;

public:
  explicit RelocatableTableIndexArg(Function *F) : F(F) {}

  [[nodiscard]] std::string getString() const override {
    return llvm::formatv("table index for {}", F->Name).str();
  }
  void encode(llvm::raw_ostream &OS) const override {
    llvm::encodeSLEB128(0, OS, 5);
  }

  void addRelocation(llvm::raw_ostream &OS, Relocation &Reloc) override {
    Reloc.Entries.emplace_back(RelocationType::R_WASM_TABLE_INDEX_SLEB,
                               OS.tell(), F->SymbolIndex);
  }
};

class WasmInst {
  using Storage =
      std::variant<std::monostate, int64_t, std::unique_ptr<InstArgument>>;
  Storage Arg;

public:
  Opcode::Enum Op;

  WasmInst(Opcode::Enum O) : Arg(std::monostate{}), Op(O) {}
  WasmInst(Opcode::Enum O, int64_t Imm) : Arg(Imm), Op(O) {}
  WasmInst(Opcode::Enum O, std::unique_ptr<InstArgument> Arg)
      : Arg(std::move(Arg)), Op(O) {}

  // Convenience constructors
  WasmInst(Opcode::Enum O, ImportedGlobal *IG)
      : Arg(std::make_unique<RelocatableGlobalArg>(IG)), Op(O) {}

  WasmInst(Opcode::Enum O, Local *L)
      : Arg(std::make_unique<LocalArg>(L)), Op(O) {}

  WasmInst(Opcode::Enum O, Data *D)
      : Arg(std::make_unique<RelocatablePointerArg>(D)), Op(O) {}

  WasmInst(Opcode::Enum O, Function *F)
      : Arg(std::make_unique<RelocatableFuncArg>(F)), Op(O) {}

  WasmInst(WasmInst &&) = default;
  WasmInst &operator=(WasmInst &&) = default;

  WasmInst(const WasmInst &) = delete;
  WasmInst &operator=(const WasmInst &) = delete;

#ifdef WATEVER_LOGGING
  [[nodiscard]] std::string getString() const {
    const char *Name = Opcode(Op).getName();

    return std::visit(
        Overloaded{
            [&](std::monostate) { return std::string(Name); },
            [&](uint64_t Imm) {
              return llvm::formatv("{0} {1}", Name, Imm).str();
            },
            [&](const std::unique_ptr<InstArgument> &Arg) {
              return llvm::formatv("{0} {1}", Name, Arg->getString()).str();
            },
        },
        Arg);
  }
#endif

  void write(llvm::raw_ostream &OS, Relocation &Reloc) const {
    Opcode(Op).writeBytes(OS);

    std::visit(Overloaded{
                   [&](std::monostate) {},
                   [&](int64_t Imm) { llvm::encodeSLEB128(Imm, OS); },
                   [&](const std::unique_ptr<InstArgument> &Arg) {
                     Arg->addRelocation(OS, Reloc);
                     Arg->encode(OS);
                   },
               },
               Arg);
  }
};

class Wasm {
public:
  virtual ~Wasm() = default;
  virtual void accept(WasmVisitor &V) { V.visit(*this); }
};

class WasmBlock final : public Wasm {
public:
  std::unique_ptr<Wasm> InnerWasm;

  explicit WasmBlock(std::unique_ptr<Wasm> Inner)
      : InnerWasm(std::move(Inner)) {};

  void accept(WasmVisitor &V) override { V.visit(*this); }
};

class WasmLoop final : public Wasm {

public:
  std::unique_ptr<Wasm> InnerWasm;
  explicit WasmLoop(std::unique_ptr<Wasm> Inner)
      : InnerWasm(std::move(Inner)) {};
  void accept(WasmVisitor &V) override { V.visit(*this); }
};

class WasmIf final : public Wasm {
public:
  std::unique_ptr<Wasm> True;
  std::unique_ptr<Wasm> False;

  explicit WasmIf(std::unique_ptr<Wasm> True, std::unique_ptr<Wasm> False)
      : True(std::move(True)), False(std::move(False)) {}
  void accept(WasmVisitor &V) override { V.visit(*this); }
};

class WasmBr final : public Wasm {
public:
  uint32_t Nesting;
  explicit WasmBr(int Nesting) : Nesting(Nesting) {}
  void accept(WasmVisitor &V) override { V.visit(*this); }
};

class WasmReturn final : public Wasm {
  void accept(WasmVisitor &V) override { V.visit(*this); }
};

class WasmActions final : public Wasm {
public:
  llvm::SmallVector<WasmInst, 8> Insts;

  void accept(WasmVisitor &V) override { V.visit(*this); }
};

class WasmSeq final : public Wasm {
public:
  std::pair<std::unique_ptr<Wasm>, std::unique_ptr<Wasm>> Flow;
  explicit WasmSeq(std::unique_ptr<Wasm> First, std::unique_ptr<Wasm> Second)
      : Flow(std::move(First), std::move(Second)) {}
  void accept(WasmVisitor &V) override { V.visit(*this); }
};

class Module {
  friend class ModuleLowering;
  uint32_t TotalGlobals{};
  uint32_t TotalTables{};
  // Helper to append raw bytes of a primitive type
  template <typename T>
  void appendBytes(std::vector<uint8_t> &Buffer, T Value) {
    const auto *Bytes = reinterpret_cast<const uint8_t *>(&Value);
    Buffer.insert(Buffer.end(), Bytes, Bytes + sizeof(T));
  }

  void flattenConstant(const llvm::Constant *C, std::vector<uint8_t> &Buffer,
                       llvm::SmallVector<RelocationEntry> &Relocs);

public:
  llvm::SmallVector<FuncType> Types;
  llvm::SmallVector<std::unique_ptr<Symbol>> Symbols;
  // Deduplication lookup table
  std::map<FuncType, uint32_t> TypeLookup;
  llvm::SmallVector<ImportedFunc *> Imports{};
  llvm::SmallVector<DefinedFunc *> Functions{};
  llvm::DenseMap<llvm::Function *, Function *> FunctionMap{};

  llvm::SmallVector<ImportedGlobal *> ImportedGlobals{};
  llvm::DenseMap<llvm::Value *, Global *> GlobalMap;
  ImportedGlobal *StackPointer;

  llvm::SmallVector<DefinedData *> Datas{};
  llvm::DenseMap<llvm::GlobalValue *, Data *> DataMap;

  std::vector<Function *> IndirectFunctionElements{nullptr};
  llvm::DenseMap<Function *, uint32_t> IndirectFunctionElementLookup;
  UndefinedTable *IndirectFunctionTable{};

  uint32_t getOrAddType(const FuncType &Signature) {
    if (auto It = TypeLookup.find(Signature); It != TypeLookup.end()) {
      return It->second;
    }
    // Type doesn't yet exist
    auto NewIndex = static_cast<uint32_t>(Types.size());
    Types.push_back(Signature);
    TypeLookup.insert({Signature, NewIndex});
    return NewIndex;
  }

  void addIndirectFunctionElement(Function *F) {
    if (IndirectFunctionElementLookup.contains(F)) {
      return;
    }
    IndirectFunctionElementLookup[F] = IndirectFunctionElements.size();
    IndirectFunctionElements.push_back(F);
  }

  /// Returns a pointer to the stack pointer global - creates one, if it doesn't
  /// exist yet.
  ImportedGlobal *getStackPointer(ValType PtrTy) {
    if (!StackPointer) {
      auto NewStackPointer = std::make_unique<ImportedGlobal>(
          Symbols.size(), TotalGlobals++, PtrTy, true, "env",
          "__stack_pointer");
      NewStackPointer->setFlag(SymbolFlag::WASM_SYM_UNDEFINED);
      StackPointer = NewStackPointer.get();
      ImportedGlobals.push_back(StackPointer);
      Symbols.push_back(std::move(NewStackPointer));
    }
    return StackPointer;
  }

  UndefinedTable *getIndirectFunctionTable() {
    if (!IndirectFunctionTable) {
      auto NewIndirectFunctionTable = std::make_unique<UndefinedTable>(
          Symbols.size(), TotalTables++, ValType::Func,
          "__indirect_function_table");
      NewIndirectFunctionTable->setFlag(SymbolFlag::WASM_SYM_NO_STRIP);
      NewIndirectFunctionTable->setFlag(SymbolFlag::WASM_SYM_UNDEFINED);
      IndirectFunctionTable = NewIndirectFunctionTable.get();
      Symbols.push_back(std::move(NewIndirectFunctionTable));
    }
    return IndirectFunctionTable;
  }
};

class BlockLowering : public llvm::InstVisitor<BlockLowering> {
  friend class llvm::InstVisitor<BlockLowering>;
  llvm::BasicBlock *BB;

  llvm::SmallVector<llvm::Value *> WorkList;

  WasmActions Actions;

  void calculateLiveOut();
  [[nodiscard]] llvm::DenseMap<llvm::Value *, int>
  getInternalUserCounts() const;

  Module &M;
  DefinedFunc *Parent;

  void addOperandsToWorklist(llvm::iterator_range<llvm::Use *> Ops) {
    for (llvm::Value *Op : Ops) {
      WorkList.push_back(Op);
    }
  }

  // Terminator Instructions (should not be needed, as these are mapped to
  // blocks)
  void visitReturnInst(llvm::ReturnInst &RI) {
    // Load saved stack pointer if needed. All potential allocas for this return
    // dominate us
    if (Parent->SavedSP) {
      ValType PointerType;
      if (RI.getModule()->getDataLayout().getPointerSizeInBits() == 64) {
        PointerType = ValType::I64;
      } else {
        PointerType = ValType::I32;
      }
      auto *StackPointer = M.getStackPointer(PointerType);
      auto GlobalArgVal = std::make_unique<RelocatableGlobalArg>(StackPointer);
      Actions.Insts.emplace_back(Opcode::GlobalSet, std::move(GlobalArgVal));
      auto SavedSPArg = std::make_unique<LocalArg>(Parent->SavedSP);
      Actions.Insts.emplace_back(Opcode::LocalGet, std::move(SavedSPArg));
    }
    addOperandsToWorklist(RI.operands());
  }

  void visitBranchInst(llvm::BranchInst &BI) {
    if (BI.isConditional()) {
      WorkList.push_back(BI.getCondition());
    }
  };

  // Unary Operations
  void visitUnaryOperator(llvm::UnaryOperator &UO);
  // Binary Operations
  void visitBinaryOperator(llvm::BinaryOperator &BO);
  // Vector Operations
  // Aggregatge Operations
  // Memory Access and Addressing Operations
  void visitAllocaInst(llvm::AllocaInst &AI);
  /// Memory instruction with \p Op. Tries to coalesce the offset into the
  /// instruction.
  void doGreedyMemOp(llvm::Instruction &I, Opcode::Enum Op);
  void visitLoadInst(llvm::LoadInst &LI);
  void visitStoreInst(llvm::StoreInst &SI);
  // Conversion Operations
  void visitTruncInst(llvm::TruncInst &TI);
  void visitZExtInst(llvm::ZExtInst &ZI);
  void visitSExtInst(llvm::SExtInst &SI);
  void visitIntToPtrInst(llvm::IntToPtrInst &I) {
    addOperandsToWorklist(I.operands());
  };
  void visitPtrToIntInst(llvm::PtrToIntInst &I) {
    addOperandsToWorklist(I.operands());
  };

  // Other Operations
  void visitICmpInst(llvm::ICmpInst &II);
  void visitFCmpInst(llvm::FCmpInst &FI);
  void visitPHINode(llvm::PHINode &PN);
  void visitSelectInst(llvm::SelectInst &SI);
  void visitCallInst(llvm::CallInst &CI);

  void visitInstruction(llvm::Instruction &I) {
    addOperandsToWorklist(I.operands());
    // TODO set to UNIMPLEMENTED
    WATEVER_TODO("{} not (yet) supported", I.getOpcodeName());
  }

public:
  explicit BlockLowering(llvm::BasicBlock *BB, Module &M, DefinedFunc *F)
      : BB(BB), M(M), Parent(F) {}

  std::unique_ptr<WasmActions> lower();
};

class FunctionLowering {
  // TODO I currently don't see why we need to store blocktype
  enum class BlockType : uint8_t {
    IfThenElse,
    Loop,
    Block,
  };
  class ContainingSyntax {
    [[maybe_unused]]
    BlockType BT;

    ContainingSyntax(BlockType BT, llvm::BasicBlock *Label)
        : BT(BT), Label(Label) {}

  public:
    llvm::BasicBlock *Label;
    static ContainingSyntax createIf() {
      return {BlockType::IfThenElse, nullptr};
    }
    static ContainingSyntax createLoop(llvm::BasicBlock *Header) {
      return {BlockType::Loop, Header};
    }
    static ContainingSyntax createBlock(llvm::BasicBlock *Follow) {
      return {BlockType::Block, Follow};
    }
  };

  DefinedFunc *F;
  using Context = llvm::SmallVector<ContainingSyntax, 8>;

  llvm::DominatorTree &DT;
  llvm::LoopInfo &LI;
  Module &M;

  std::unique_ptr<Wasm> doBranch(const llvm::BasicBlock *SourceBlock,
                                 llvm::BasicBlock *TargetBlock, Context Ctx);

  // TODO MergeChildren needs better type
  std::unique_ptr<Wasm>
  nodeWithin(llvm::BasicBlock *Parent,
             llvm::SmallVector<llvm::BasicBlock *> MergeChildren,
             const Context &Ctx);

  std::unique_ptr<Wasm> doTree(llvm::BasicBlock *Root, Context Ctx);

  static int index(const llvm::BasicBlock *BB, Context &Ctx);

  std::unique_ptr<WasmActions> translateBB(llvm::BasicBlock *BB) const;

  void
  getMergeChildren(const llvm::BasicBlock *R,
                   llvm::SmallVectorImpl<llvm::BasicBlock *> &Result) const;

  static bool isMergeNode(const llvm::BasicBlock *BB) {
    return !llvm::pred_empty(BB) && !BB->getSinglePredecessor();
  }

public:
  FunctionLowering(DefinedFunc *F, llvm::DominatorTree &DT, llvm::LoopInfo &LI,
                   Module &M)
      : F(F), DT(DT), LI(LI), M(M) {}

  void lower() {
    Context Ctx;
    F->Body = doTree(DT.getRoot(), Ctx);
  }
};

class ModuleLowering {
public:
  static Module convert(llvm::Module &Mod, llvm::FunctionAnalysisManager &FAM);
};

} // namespace watever
