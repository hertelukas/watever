#pragma once

#include "watever/opcode.hpp"
#include "watever/utils.hpp"
#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/SmallVector.h>
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

class WasmInst {
  enum class Format : uint8_t { None, Inline, Complex };
  Format Fmt;
  union {
    uint64_t Imm;
    void *Data;
  };

public:
  Opcode::Enum Op;

  WasmInst(Opcode::Enum O) : Fmt(Format::None), Imm(0), Op(O) {}
  WasmInst(Opcode::Enum O, uint64_t Imm)
      : Fmt(Format::Inline), Imm(Imm), Op(O) {}

  std::string getString() const {
    const char *Name = Opcode(Op).getName();
    switch (Fmt) {
    case Format::Inline:
      return llvm::formatv("{0} {1}", Name, Imm).str();
    case Format::Complex:
      return std::string(Name) + " [complex]";
    default:
      return Name;
    }
  }

  void write(llvm::raw_svector_ostream &OS) {
    Opcode(Op).writeBytes(OS);
    switch (Fmt) {

    case Format::None:
      break;
    case Format::Inline:
      llvm::encodeULEB128(Imm, OS);
      break;
    case Format::Complex:
      WATEVER_UNIMPLEMENTED("Handle complex parameters");
      break;
    }
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

// TODO
struct StructType {
  auto operator<=>(const StructType &) const = default;
};

// TODO
struct ArrayType {
  auto operator<=>(const ArrayType &) const = default;
};

struct FuncType {
  // TODO not sure if "Type" is correct here (should be valtype)
  std::vector<Type::Enum> Args;
  std::vector<Type::Enum> Results;

  auto operator<=>(const FuncType &) const = default;
};

// TODO support typeuse
struct SubType {
  bool IsFinal;
  std::variant<FuncType, StructType, ArrayType> Composite;
  uint32_t Index;

  auto operator<=>(const SubType &) const = default;

  SubType(FuncType FT) : Composite(FT) {}
  SubType(StructType ST) : Composite(ST) {}
  SubType(ArrayType AT) : Composite(AT) {}
};

class Function {
  friend class FunctionLowering;
  uint32_t TotalLocals{};

public:
  llvm::StringRef Name;
  std::unique_ptr<Wasm> Body{};
  llvm::DenseMap<Type::Enum, uint32_t> Locals{};
  const SubType *TypePtr{};
  uint32_t Index;

  explicit Function(const SubType *Type, uint32_t Args, llvm::StringRef Name)
      : TotalLocals(Args), Name(Name), TypePtr(Type) {}

  int getNewLocal(Type::Enum Ty) {
    Locals[Ty]++;
    return TotalLocals++;
  }

  void visit(WasmVisitor &V) const { Body->accept(V); }
};

class Module {
public:
  // Canonical storage for types
  std::map<SubType, std::unique_ptr<SubType>> Types;
  llvm::SmallVector<Function, 4> Functions;

  const SubType *getOrAddType(const SubType &Temp) {
    auto It = Types.find(Temp);
    if (It != Types.end())
      return It->second.get();

    auto NewType = std::make_unique<SubType>(Temp);
    const SubType *Ptr = NewType.get();
    Types.emplace(Temp, std::move(NewType));
    return Ptr;
  }
};

class BlockLowering : public llvm::InstVisitor<BlockLowering> {
  friend class llvm::InstVisitor<BlockLowering>;
  llvm::BasicBlock *BB;

  WasmActions Actions;
  llvm::DenseMap<llvm::Value *, int> LocalMapping;

  llvm::SmallVector<llvm::Value *> getLiveOut() const;
  llvm::DenseMap<llvm::Value *, int> getInternalUserCounts() const;

  // Terminator Instructions (should not be needed, as these are mapped to
  // blocks)
  void visitReturnInst(llvm::ReturnInst &) {};
  void visitBranchInst(llvm::BranchInst &) {};
  // Unary Operations
  void visitUnaryOperator(llvm::UnaryOperator &UO);
  // Binary Operations
  void visitBinaryOperator(llvm::BinaryOperator &BO);
  // Vector Operations
  // Aggregatge Operations
  // Memory Access and Addressing Operations
  // Conversion Operations
  void visitSExtInst(llvm::SExtInst &SI);
  void visitIntToPtrInst(llvm::IntToPtrInst &) {};
  void visitPtrToIntInst(llvm::PtrToIntInst &) {};

  // Other Operations
  void visitFCmpInst(llvm::FCmpInst &FI);
  void visitICmpInst(llvm::ICmpInst &II);

  void visitInstruction(llvm::Instruction &I) {
    // TODO set to UNIMPLEMENTED
    WATEVER_TODO("{} not (yet) supported", I.getOpcodeName());
  }

public:
  explicit BlockLowering(llvm::BasicBlock *BB) : BB(BB) {}

  std::unique_ptr<WasmActions> lower(Function &F);
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

  Function &F;
  using Context = llvm::SmallVector<ContainingSyntax, 8>;

  llvm::DominatorTree &DT;
  llvm::LoopInfo &LI;

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
  FunctionLowering(Function &F, llvm::DominatorTree &DT, llvm::LoopInfo &LI)
      : F(F), DT(DT), LI(LI) {}

  void lower() {
    Context Ctx;
    F.Body = doTree(DT.getRoot(), Ctx);
  }
};

class ModuleLowering {

public:
  static Module convert(llvm::Module &Mod, llvm::FunctionAnalysisManager &FAM);
};

} // namespace watever
