#ifndef IR_H
#define IR_H

#include "watever/opcode.h"
#include <llvm/ADT/SmallVector.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/IR/Analysis.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/Support/raw_ostream.h>
#include <memory>
#include <vector>

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
  virtual void visit(Wasm &) {};
  virtual void visit(WasmBlock &) = 0;
  virtual void visit(WasmLoop &) = 0;
  virtual void visit(WasmIf &) = 0;
  virtual void visit(WasmBr &) = 0;
  virtual void visit(WasmReturn &) = 0;
  virtual void visit(WasmActions &) = 0;
  virtual void visit(WasmSeq &) = 0;
};

struct WasmInstruction {
  Opcode Op;
  llvm::SmallVector<WasmInstruction *, 4> Operands;
};

class Wasm {
public:
  virtual ~Wasm() = default;
  virtual void accept(WasmVisitor &V) { V.visit(*this); }
};

class WasmBlock : public Wasm {
public:
  std::unique_ptr<Wasm> InnerWasm;

  explicit WasmBlock(std::unique_ptr<Wasm> Inner)
      : InnerWasm(std::move(Inner)) {};

  virtual void accept(WasmVisitor &V) override { V.visit(*this); }
};

class WasmLoop : public Wasm {

public:
  std::unique_ptr<Wasm> InnerWasm;
  explicit WasmLoop(std::unique_ptr<Wasm> Inner)
      : InnerWasm(std::move(Inner)) {};
  virtual void accept(WasmVisitor &V) override { V.visit(*this); }
};

class WasmIf : public Wasm {
public:
  std::unique_ptr<Wasm> True;
  std::unique_ptr<Wasm> False;

  explicit WasmIf(std::unique_ptr<Wasm> True, std::unique_ptr<Wasm> False)
      : True(std::move(True)), False(std::move(False)) {}
  virtual void accept(WasmVisitor &V) override { V.visit(*this); }
};

class WasmBr : public Wasm {
public:
  int Nesting;
  explicit WasmBr(int Nesting) : Nesting(Nesting) {}
  virtual void accept(WasmVisitor &V) override { V.visit(*this); }
};

class WasmReturn : public Wasm {};

class WasmActions : public Wasm {
  std::vector<std::unique_ptr<WasmInstruction>> Roots;
  virtual void accept(WasmVisitor &V) override { V.visit(*this); }
};

class WasmSeq : public Wasm {
public:
  std::pair<std::unique_ptr<Wasm>, std::unique_ptr<Wasm>> Flow;
  explicit WasmSeq(std::unique_ptr<Wasm> First, std::unique_ptr<Wasm> Second)
      : Flow(std::move(First), std::move(Second)) {}
  virtual void accept(WasmVisitor &V) override { V.visit(*this); }
};

class Function {};
class Module {
public:
  llvm::SmallVector<Function, 4> Functions;
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

  using Context = llvm::SmallVector<ContainingSyntax, 8>;

  llvm::DominatorTree &DT;
  llvm::LoopInfo &LI;

  std::unique_ptr<Wasm> doBranch(llvm::BasicBlock *SourceBlock,
                                 llvm::BasicBlock *TargetBlock, Context Ctx);

  // TODO MergeChildren needs better type
  std::unique_ptr<Wasm>
  nodeWithin(llvm::BasicBlock *Parent,
             llvm::SmallVector<llvm::BasicBlock *> MergeChildren, Context Ctx);

  std::unique_ptr<Wasm> doTree(llvm::BasicBlock *Root, Context Ctx);

  int index(llvm::BasicBlock *BB, Context &Ctx);

  std::unique_ptr<WasmActions> translateBB(llvm::BasicBlock *BB);

  void getMergeChildren(llvm::BasicBlock *R,
                        llvm::SmallVectorImpl<llvm::BasicBlock *> &Result);

  static bool isMergeNode(const llvm::BasicBlock *BB) {
    return !llvm::pred_empty(BB) && !BB->getSinglePredecessor();
  }

public:
  FunctionLowering(llvm::DominatorTree &DT, llvm::LoopInfo &LI)
      : DT(DT), LI(LI) {}

  std::unique_ptr<Wasm> lower() {
    Context Ctx;
    return doTree(DT.getRoot(), Ctx);
  }
};

class ModuleLowering {

public:
  Module convert(llvm::Module &Mod, llvm::FunctionAnalysisManager &FAM);
};

class WasmPrinter : public WasmVisitor {
  unsigned int Depth = 0;

  void print(llvm::StringRef Text) {
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

  void visit(WasmActions &Actions) override { print("actions"); }

  void visit(WasmBr &Br) override { print("br " + std::to_string(Br.Nesting)); }
};
} // namespace watever

#endif /* IR_H */
