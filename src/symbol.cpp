#include "watever/symbol.hpp"
#include "watever/ir.hpp"
#include "watever/type.hpp"
#include <algorithm>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Instructions.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Support/MathExtras.h>

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

void DefinedFunc::setupStackFrame(llvm::BasicBlock *Entry) {
  llvm::SmallVector<llvm::AllocaInst *> StaticAllocas;

  for (auto &Inst : *Entry) {
    if (auto *AI = llvm::dyn_cast<llvm::AllocaInst>(&Inst)) {
      if (AI->isStaticAlloca())
        StaticAllocas.push_back(AI);
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
