#include "watever/symbol.hpp"
#include "watever/feature.hpp"
#include "watever/type.hpp"
#include <algorithm>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Instructions.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Support/MathExtras.h>
#include <utility>

using namespace watever;

DefinedGlobal::DefinedGlobal(uint32_t SymbolIdx, uint32_t GlobalIdx, ValType Ty,
                             bool Mut, WasmActions Ex)
    : Global(Kind::DefinedGlobal, SymbolIdx, GlobalIdx, Ty, Mut),
      Expr(std::move(Ex)) {}

DefinedFunc::DefinedFunc(uint32_t SymbolIdx, uint32_t TypeIdx, uint32_t FuncIdx,
                         llvm::Function *F, Features Feat)
    : Function(Kind::DefinedFunc, SymbolIdx, TypeIdx, FuncIdx,
               F->getName().str()),
      TotalArgs(F->arg_size()), FeatureSet(Feat) {
  for (auto &Arg : F->args()) {
    auto ArgType = fromLLVMType(Arg.getType(), F->getDataLayout());
    auto NewLocal = LastLocal++;
    Arguments[ArgType].push_back(NewLocal);
    LocalMapping[&Arg] = NewLocal;
  }
}

bool DefinedFunc::hasExternalUser(llvm::Value *Val, llvm::BasicBlock *BB) {
  // If this instruction is a promoted alloca, it alrady has a local and does
  // not need to be materialized for external users
  if (auto *AI = llvm::dyn_cast<llvm::AllocaInst>(Val)) {
    if (PromotedAllocas.contains(AI)) {
      return false;
    }
  }

  for (auto *User : Val->users()) {
    if (auto *Inst = llvm::dyn_cast<llvm::Instruction>(User)) {
      if (Inst->getParent() != BB || llvm::isa<llvm::PHINode>(Inst)) {
        return true;
      }
    }
  }
  return false;
}

void DefinedFunc::setupStackFrame(llvm::BasicBlock *Entry) {
  llvm::SmallVector<llvm::AllocaInst *> StaticAllocas;

  for (auto &Inst : *Entry) {
    if (auto *AI = llvm::dyn_cast<llvm::AllocaInst>(&Inst)) {
      if (AI->isStaticAlloca()) {
        if (!PromotedAllocas.contains(AI)) {
          StaticAllocas.push_back(AI);
        }
      }
    }
  }

  std::ranges::sort(StaticAllocas, [](auto *A, auto *B) {
    return A->getAlign() > B->getAlign();
  });

  // Offset from original SP
  int64_t CurrentOffset = 0;
  for (auto *AI : StaticAllocas) {
    llvm::Align Alignment = AI->getAlign();
    auto *AllocatedType = AI->getAllocatedType();
    uint64_t TypeSize = Entry->getDataLayout().getTypeAllocSize(AllocatedType);
    auto *ArraySize = llvm::cast<llvm::ConstantInt>(AI->getArraySize());
    uint64_t TotalSize = TypeSize * ArraySize->getZExtValue();

    CurrentOffset -= TotalSize;

    // Align down
    CurrentOffset = -llvm::alignTo(-CurrentOffset, Alignment);
    StackSlots[AI] = CurrentOffset;
  }

  if (CurrentOffset != 0) {
    FrameSize = llvm::alignTo(-CurrentOffset, 16);
    FP = getNewLocal(Entry->getDataLayout().getPointerSizeInBits() == 64
                         ? ValType::I64
                         : ValType::I32);
    // Convert FP relative to SP relative (positive)
    for (auto *AI : StaticAllocas) {
      StackSlots[AI] += FrameSize;
    }
  }
}
