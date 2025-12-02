#ifndef IR_H
#define IR_H

#include "watever/opcode.h"
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
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/FormatVariadic.h>
#include <llvm/Support/raw_ostream.h>
#include <memory>

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
  int Nesting;
  explicit WasmBr(int Nesting) : Nesting(Nesting) {}
  void accept(WasmVisitor &V) override { V.visit(*this); }
};

class WasmReturn final : public Wasm {};

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

class Function {
  friend class FunctionLowering;
  int LastLocal = 0;
  std::unique_ptr<Wasm> Body;

public:
  explicit Function(int NumArgs) : LastLocal(NumArgs) {}
  int getNewLocal() { return LastLocal++; }

  void visit(WasmVisitor &V) { Body->accept(V); }
};

class Module {
public:
  llvm::SmallVector<Function, 4> Functions;
};

class BlockLowering : public llvm::InstVisitor<BlockLowering> {
  friend class llvm::InstVisitor<BlockLowering>;
  llvm::BasicBlock *BB;

  WasmActions Actions;
  llvm::DenseMap<llvm::Value *, int> LocalMapping;

  llvm::SmallVector<llvm::Value *> getLiveOut() const;
  llvm::DenseMap<llvm::Value *, int> getInternalUserCounts() const;

  void visitBinaryOperator(llvm::BinaryOperator &BO);
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
             llvm::SmallVector<llvm::BasicBlock *> MergeChildren, const Context &Ctx);

  std::unique_ptr<Wasm> doTree(llvm::BasicBlock *Root, Context Ctx);

  static int index(const llvm::BasicBlock *BB, Context &Ctx);

  std::unique_ptr<WasmActions> translateBB(llvm::BasicBlock *BB) const;

  void getMergeChildren(const llvm::BasicBlock *R,
                        llvm::SmallVectorImpl<llvm::BasicBlock *> &Result) const;

  static bool isMergeNode(const llvm::BasicBlock *BB) {
    return !llvm::pred_empty(BB) && !BB->getSinglePredecessor();
  }

public:
  FunctionLowering(Function &F, llvm::DominatorTree &DT, llvm::LoopInfo &LI)
      : DT(DT), LI(LI), F(F) {}

  void lower() {
    Context Ctx;
    F.Body = doTree(DT.getRoot(), Ctx);
  }
};

class ModuleLowering {

public:
  static Module convert(llvm::Module &Mod, llvm::FunctionAnalysisManager &FAM);
};

class WasmPrinter final : public WasmVisitor {
  unsigned int Depth = 0;

  void print(llvm::StringRef Text) const {
    for (size_t I = 0; I < Depth; ++I) {
      llvm::outs() << "|  ";
    }

    llvm::outs() << Text << "\n";
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

  void visit(WasmReturn &Ret) override { print("ret"); }

  void visit(WasmSeq &Seq) override {
    Seq.Flow.first->accept(*this);
    Seq.Flow.second->accept(*this);
  }

  void visit(WasmActions &Actions) override {
    for (auto Inst : Actions.Insts) {
      print(Inst.getString());
    }
  }

  void visit(WasmBr &Br) override { print("br " + std::to_string(Br.Nesting)); }
};
} // namespace watever

#endif /* IR_H */
