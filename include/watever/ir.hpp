#pragma once

#include "watever/instructions.hpp"
#include "watever/linking.hpp"
#include "watever/opcode.hpp"
#include "watever/symbol.hpp"
#include "watever/target.hpp"
#include "watever/type.hpp"
#include "watever/utils.hpp"
#include <cstdint>
#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/PostOrderIterator.h>
#include <llvm/ADT/SmallPtrSet.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/iterator_range.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/IR/Analysis.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/DataLayout.h>
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

namespace watever {
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

  void flattenConstant(
      const llvm::Constant *C, std::vector<uint8_t> &Buffer,
      llvm::SmallVector<std::unique_ptr<RelocationEntry>> &Relocs,
      llvm::DenseMap<RelocationEntry *, const llvm::GlobalValue *> &FixUps,
      const llvm::DataLayout &DL);

public:
  TargetConfig Config;
  llvm::SmallVector<FuncType> Types;
  llvm::SmallVector<std::unique_ptr<Symbol>> Symbols;
  // Deduplication lookup table
  std::map<FuncType, uint32_t> TypeLookup;
  llvm::SmallVector<ImportedFunc *> Imports{};
  llvm::SmallVector<DefinedFunc *> Functions{};
  llvm::DenseMap<const llvm::Function *, Function *> FunctionMap{};

  llvm::SmallVector<ImportedGlobal *> ImportedGlobals{};
  llvm::DenseMap<llvm::Value *, Global *> GlobalMap;
  ImportedGlobal *StackPointer = nullptr;

  llvm::SmallVector<DefinedData *> Datas{};
  llvm::DenseMap<const llvm::GlobalValue *, Data *> DataMap;

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

  // Keeps track of indices in the worklist, where the alloca instruction should
  // not emit add + offset, as the offset is already encoded in the load/store
  llvm::SmallVector<size_t> AllocaSkipOffsetList;

  WasmActions Actions;

  llvm::SmallPtrSet<llvm::Instruction *, 16> Emitted;

  /// Calculates the users of each value for the AST at \p Root.
  [[nodiscard]] llvm::DenseMap<llvm::Value *, int>
  getDependencyTreeUserCount(llvm::Instruction *Root) const;

  Module &M;
  DefinedFunc &Parent;

  void addOperandsToWorklist(llvm::iterator_range<llvm::Use *> Ops) {
    for (llvm::Value *Op : Ops) {
      WorkList.push_back(Op);
    }
  }

  void handleIntrinsic(llvm::CallInst &CI);

  // Terminator Instructions (should not be needed, as these are mapped to
  // blocks)
  void visitReturnInst(llvm::ReturnInst &RI) {
    if (Parent.FP.has_value()) {
      // Eplilogue is SP = FP + static_stack_size
      const bool Is64Bit =
          RI.getModule()->getDataLayout().getPointerSizeInBits() == 64;
      const auto PtrTy = Is64Bit ? ValType::I64 : ValType::I32;
      const auto ConstOp = Is64Bit ? Opcode::I64Const : Opcode::I32Const;
      const auto AddOp = Is64Bit ? Opcode::I64Add : Opcode::I32Add;
      auto *StackPointer = M.getStackPointer(PtrTy);
      auto GlobalArgVal = std::make_unique<RelocatableGlobalArg>(StackPointer);
      Actions.Insts.emplace_back(Opcode::GlobalSet, std::move(GlobalArgVal));
      Actions.Insts.emplace_back(AddOp);
      Actions.Insts.emplace_back(ConstOp, Parent.FrameSize);
      auto SavedSPArg = std::make_unique<LocalArg>(Parent.FP.value());
      Actions.Insts.emplace_back(Opcode::LocalGet, std::move(SavedSPArg));
    }
    addOperandsToWorklist(RI.operands());
  }

  void visitBranchInst(llvm::BranchInst &BI) {
    if (BI.isConditional()) {
      WorkList.push_back(BI.getCondition());
    }
  };

  void visitSwitchInst(llvm::SwitchInst &SI) {
    WorkList.push_back(SI.getCondition());
  }

  void visitUnreachableInst(llvm::UnreachableInst &) {}

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
  void visitFPTruncInst(llvm::FPTruncInst &FI);
  void visitFPExtInst(llvm::FPExtInst &FI);
  void visitFPToUIInst(llvm::FPToUIInst &FI);
  void visitFPToSIInst(llvm::FPToSIInst &FI);
  void visitUIToFPInst(llvm::UIToFPInst &UI);
  void visitSIToFPInst(llvm::SIToFPInst &SI);

  void visitPtrToIntInst(llvm::PtrToIntInst &I) {
    addOperandsToWorklist(I.operands());
  };

  void visitIntToPtrInst(llvm::IntToPtrInst &I) {
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
  explicit BlockLowering(llvm::BasicBlock *BB, Module &M, DefinedFunc &F)
      : BB(BB), M(M), Parent(F) {}

  void lower();
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
    static ContainingSyntax createIf(llvm::BasicBlock *Follow) {
      return {BlockType::IfThenElse, Follow};
    }
    static ContainingSyntax createLoop(llvm::BasicBlock *Header) {
      return {BlockType::Loop, Header};
    }
    static ContainingSyntax createBlock(llvm::BasicBlock *Follow) {
      return {BlockType::Block, Follow};
    }
  };

  DefinedFunc &F;
  struct Context {
    llvm::SmallVector<ContainingSyntax> Enclosing;
    // The label that can be reached by just falling through
    llvm::BasicBlock *Fallthrough = nullptr;
  };

  llvm::DominatorTree &DT;
  llvm::LoopInfo &LI;
  Module &M;

  void doBranch(const llvm::BasicBlock *SourceBlock,
                llvm::BasicBlock *TargetBlock, const Context &Ctx, ValType FTy);

  // TODO MergeChildren needs better type
  void nodeWithin(llvm::BasicBlock *Parent,
                  llvm::SmallVector<llvm::BasicBlock *> MergeChildren,
                  const Context &Ctx, ValType FTy, llvm::BasicBlock *Follow);

  void doTree(llvm::BasicBlock *Root, Context Ctx, ValType FTy);

  static uint32_t index(const llvm::BasicBlock *BB, const Context &Ctx);

  void translateBB(llvm::BasicBlock *BB) const;

  void
  getMergeChildren(const llvm::BasicBlock *R,
                   llvm::SmallVectorImpl<llvm::BasicBlock *> &Result) const;

  bool isMergeNode(const llvm::BasicBlock *BB) const {
    // If we have only one, or no predecessor, we are cleary not a merge node
    if (llvm::pred_empty(BB) || BB->getSinglePredecessor()) {
      return false;
    }

    unsigned ForwardEdges = 0;
    for (auto *Pred : llvm::predecessors(BB)) {
      // if BB dominates Pred, we are looking at a back edge; we are only
      // interested in forward edges
      if (!DT.dominates(BB, Pred)) {
        ForwardEdges++;
      }
    }
    return ForwardEdges >= 2;
  }

  // reverse post order
  llvm::DenseMap<llvm::BasicBlock *, uint64_t> RPOOrdering;

public:
  FunctionLowering(DefinedFunc &F, llvm::DominatorTree &DT, llvm::LoopInfo &LI,
                   Module &M)
      : F(F), DT(DT), LI(LI), M(M) {
    uint64_t Idx = 0;
    llvm::ReversePostOrderTraversal<llvm::Function *> PROT(
        DT.getRoot()->getParent());
    for (auto *BB : PROT) {
      RPOOrdering[BB] = Idx++;
    }
  }

  void lower(ValType RetTy) {
    Context Ctx;
    doTree(DT.getRoot(), Ctx, RetTy);
  }
};

class ModuleLowering {
public:
  static Module convert(llvm::Module &Mod, llvm::FunctionAnalysisManager &FAM,
                        TargetConfig C);
};

} // namespace watever
