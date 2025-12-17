#pragma once

#include "watever/linking.hpp"
#include "watever/opcode.hpp"
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
  virtual std::string getString() const = 0;
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

  std::string getString() const override {
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
  uint32_t Index;
  explicit LocalArg(uint32_t Index) : Index(Index) {}
  std::string getString() const override { return llvm::formatv("{}", Index); }
  void encode(llvm::raw_ostream &OS) const override {
    llvm::encodeULEB128(Index, OS);
  }
};

class RelocatableFuncArg final : public InstArgument {
public:
  uint32_t FunctionIndex;
  explicit RelocatableFuncArg(uint32_t Index) : FunctionIndex(Index) {}
  std::string getString() const override {
    return llvm::formatv("{}", FunctionIndex);
  }
  void encode(llvm::raw_ostream &OS) const override {
    // Must be padded to 5 bytes, so it can be patched by the linker
    llvm::encodeULEB128(FunctionIndex, OS, 5);
  }

  // TODO using the functionIndex is not generally correct, it should be the
  // symbol table index
  void addRelocation(llvm::raw_ostream &OS, Relocation &Reloc) override {
    Reloc.Entries.emplace_back(RelocationType::R_WASM_FUNCTION_INDEX_LEB,
                               OS.tell(), FunctionIndex);
  }
};

class WasmInst {
  // TODO remove last option; create one consistent, non-owning InstArgument
  // pointer. See Issue #4
  using Storage = std::variant<std::monostate, int64_t,
                               std::unique_ptr<InstArgument>, LocalArg *>;
  Storage Data;

public:
  Opcode::Enum Op;

  WasmInst(Opcode::Enum O) : Data(std::monostate{}), Op(O) {}
  WasmInst(Opcode::Enum O, int64_t Imm) : Data(Imm), Op(O) {}
  WasmInst(Opcode::Enum O, std::unique_ptr<InstArgument> Arg)
      : Data(std::move(Arg)), Op(O) {}
  // FIXME this should be deleted, and united with the above constructor
  WasmInst(Opcode::Enum O, LocalArg *Arg) : Data(Arg), Op(O) {}

  WasmInst(WasmInst &&) = default;
  WasmInst &operator=(WasmInst &&) = default;

  WasmInst(const WasmInst &) = delete;
  WasmInst &operator=(const WasmInst &) = delete;

#ifdef WATEVER_LOGGING
  std::string getString() const {
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
            [&](LocalArg *Local) {
              return llvm::formatv("{0} {1}", Name, Local->getString()).str();
            }},
        Data);
  }
#endif

  void write(llvm::raw_ostream &OS, Relocation &Reloc) const {
    Opcode(Op).writeBytes(OS);

    std::visit(Overloaded{[&](std::monostate) {},
                          [&](int64_t Imm) { llvm::encodeSLEB128(Imm, OS); },
                          [&](const std::unique_ptr<InstArgument> &Arg) {
                            Arg->addRelocation(OS, Reloc);
                            Arg->encode(OS);
                          },
                          [&](LocalArg *Local) { Local->encode(OS); }},
               Data);
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

struct Function {
  uint32_t LinkerFlags{};
  std::string Name;
  uint32_t TypeIndex;
  uint32_t FunctionIndex;

  Function(uint32_t TypeIdx, uint32_t FuncIdx, std::string Nm)
      : Name(std::move(Nm)), TypeIndex(TypeIdx), FunctionIndex(FuncIdx) {}

  void setFlag(SymbolFlag Flag) { LinkerFlags |= static_cast<uint32_t>(Flag); }
  bool isSet(SymbolFlag Flag) {
    return (LinkerFlags & static_cast<uint32_t>(Flag)) != 0;
  }

  // Needed to decide if we have to write out the SymbolName in the relocation
  virtual bool isImport() = 0;

  virtual ~Function() = default;
};

struct ImportedFunc final : public Function {
  explicit ImportedFunc(uint32_t TypeIdx, uint32_t FuncIdx,
                        llvm::StringRef Name)
      : Function(TypeIdx, FuncIdx, Name.str()) {}

  bool isImport() { return true; }
};

class DefinedFunc final : public Function {
  friend class FunctionLowering;
  uint32_t TotalLocals{};

public:
  uint32_t Args{};
  std::unique_ptr<Wasm> Body{};
  llvm::DenseMap<llvm::Value *, LocalArg *> LocalMapping;
  llvm::DenseMap<ValType, llvm::SmallVector<std::unique_ptr<LocalArg>>>
      Locals{};

  explicit DefinedFunc(uint32_t TypeIdx, uint32_t FuncIdx, uint32_t Args,
                       llvm::StringRef Name)
      : Function(TypeIdx, FuncIdx, Name.str()), Args(Args) {}

  LocalArg *getNewLocal(ValType Ty) {
    auto NewLocal = std::make_unique<LocalArg>(TotalLocals++);
    auto *Result = NewLocal.get();
    Locals[Ty].push_back(std::move(NewLocal));
    // Assign temporary local for debugging
    return Result;
  }

  bool isImport() { return false; }
  void visit(WasmVisitor &V) const { Body->accept(V); }
};

class Module {
public:
  llvm::SmallVector<FuncType> Types;
  // Deduplication lookup table
  std::map<FuncType, uint32_t> TypeLookup;
  llvm::SmallVector<std::unique_ptr<ImportedFunc>> Imports{};
  llvm::SmallVector<std::unique_ptr<DefinedFunc>, 4> Functions{};
  llvm::DenseMap<llvm::Function *, Function *> FunctionMap{};

  uint32_t getOrAddType(const FuncType &Signature) {
    if (auto It = TypeLookup.find(Signature); It != TypeLookup.end()) {
      return It->second;
    }
    // Type doesn't yet exist
    uint32_t NewIndex = static_cast<uint32_t>(Types.size());
    Types.push_back(Signature);
    TypeLookup.insert({Signature, NewIndex});
    return NewIndex;
  }
};

class BlockLowering : public llvm::InstVisitor<BlockLowering> {
  friend class llvm::InstVisitor<BlockLowering>;
  llvm::BasicBlock *BB;

  llvm::SmallVector<llvm::Value *> WorkList;

  WasmActions Actions;

  void calculateLiveOut();
  llvm::DenseMap<llvm::Value *, int> getInternalUserCounts() const;

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
  enum class BlockType {
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
