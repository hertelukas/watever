#include "watever/ir.hpp"
#include "watever/color.hpp"
#include "watever/instructions.hpp"
#include "watever/linking.hpp"
#include "watever/opcode.hpp"
#include "watever/printer.hpp"
#include "watever/symbol.hpp"
#include "watever/target.hpp"
#include "watever/type.hpp"
#include "watever/utils.hpp"
#include <algorithm>
#include <cstdint>
#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/DenseSet.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/Analysis/LoopNestAnalysis.h>
#include <llvm/IR/Argument.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/Operator.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/MathExtras.h>
#include <memory>
#include <optional>
#include <ranges>

using namespace watever;

/// Returns true, if \p Val has been put successfully on top of the stack,
/// false otherwise.
static bool putValueOnStack(llvm::Value *Val, WasmActions &Actions, Module &M,
                            bool Is64Bit) {
  if (auto *GV = llvm::dyn_cast<llvm::GlobalVariable>(Val)) {
    auto *WasmData = M.DataMap[GV];
    if (!WasmData) {
      WATEVER_UNREACHABLE("unknown global {}", llvmToString(*GV));
    }
    if (Is64Bit) {
      Actions.Insts.emplace_back(Opcode::I64Const, WasmData);
    } else {
      Actions.Insts.emplace_back(Opcode::I32Const, WasmData);
    }
    return true;
  }

  // Otherwise, we can just load constants/arguments
  if (const auto *Const = llvm::dyn_cast<llvm::ConstantInt>(Val)) {
    if (Const->getBitWidth() == 1) {
      Actions.Insts.push_back(
          WasmInst(Opcode::I32Const,
                   static_cast<int64_t>(Const->getValue().getZExtValue())));
    } else if (Const->getBitWidth() == 32) {
      Actions.Insts.push_back(
          WasmInst(Opcode::I32Const, Const->getValue().getSExtValue()));
    } else if (Const->getBitWidth() == 64) {
      Actions.Insts.push_back(
          WasmInst(Opcode::I64Const, Const->getValue().getSExtValue()));
    } else {
      WATEVER_UNREACHABLE("unsupported constant bit width: {}",
                          Const->getBitWidth());
    }
    return true;
  }

  if (const auto *FConst = llvm::dyn_cast<llvm::ConstantFP>(Val)) {
    if (FConst->getType()->isFloatTy()) {
      Actions.Insts.emplace_back(Opcode::F32Const,
                                 FConst->getValue().convertToFloat());
    } else if (FConst->getType()->isDoubleTy()) {
      Actions.Insts.emplace_back(Opcode::F64Const,
                                 FConst->getValue().convertToDouble());
    } else {
      WATEVER_UNREACHABLE("unsupported float type: {}",
                          llvmToString(*FConst->getType()));
    }
    return true;
  }

  if (auto *F = llvm::dyn_cast<llvm::Function>(Val)) {
    auto *WasmFunc = M.FunctionMap[F];
    M.addIndirectFunctionElement(WasmFunc);
    auto Arg = std::make_unique<RelocatableTableIndexArg>(WasmFunc);
    if (F->getDataLayout().getPointerSizeInBits() == 64) {
      Actions.Insts.emplace_back(Opcode::I64Const, std::move(Arg));
    } else {
      Actions.Insts.emplace_back(Opcode::I32Const, std::move(Arg));
    }
    return true;
  }

  if (llvm::isa<llvm::ConstantPointerNull>(Val)) {
    if (Is64Bit) {
      Actions.Insts.emplace_back(Opcode::I64Const, int64_t{0});
    } else {
      Actions.Insts.emplace_back(Opcode::I32Const, int64_t{0});
    }
    return true;
  }

  if (llvm::isa<llvm::UndefValue>(Val) || llvm::isa<llvm::PoisonValue>(Val)) {
    llvm::Type *Ty = Val->getType();

    if (Ty->isPointerTy()) {
      if (Is64Bit) {
        Actions.Insts.emplace_back(Opcode::I64Const, int64_t{0});
      } else {
        Actions.Insts.emplace_back(Opcode::I32Const, int64_t{0});
      }
    } else if (Ty->isIntegerTy()) {
      if (Ty->getIntegerBitWidth() <= 32) {
        Actions.Insts.emplace_back(Opcode::I32Const, int64_t{0});
      } else {
        Actions.Insts.emplace_back(Opcode::I64Const, int64_t{0});
      }
    } else if (Ty->isFloatTy()) {
      Actions.Insts.emplace_back(Opcode::F32Const, 0.0f);
    } else if (Ty->isDoubleTy()) {
      Actions.Insts.emplace_back(Opcode::F64Const, 0.0);
    } else {
      WATEVER_UNREACHABLE("unsupported type for undef/poison: {}",
                          llvmToString(*Ty));
    }
    return true;
  }
  return false;
}

void Module::flattenConstant(
    const llvm::Constant *C, std::vector<uint8_t> &Buffer,
    llvm::SmallVector<std::unique_ptr<RelocationEntry>> &Relocs,
    llvm::DenseMap<RelocationEntry *, const llvm::GlobalValue *> &FixUps,
    const llvm::DataLayout &DL) {
  // Simple data arrays
  if (auto *CDS = llvm::dyn_cast<llvm::ConstantDataSequential>(C)) {
    llvm::StringRef RawData = CDS->getRawDataValues();
    Buffer.insert(Buffer.end(), RawData.begin(), RawData.end());
    return;
  }

  // Aggregates
  if (auto *CA = llvm::dyn_cast<llvm::ConstantAggregate>(C)) {
    // TODO respect padding
    for (unsigned I = 0; I != CA->getNumOperands(); ++I) {
      flattenConstant(CA->getOperand(I), Buffer, Relocs, FixUps, DL);
    }
    return;
  }

  // Integer constants
  if (auto *CI = llvm::dyn_cast<llvm::ConstantInt>(C)) {
    unsigned Width = CI->getBitWidth();
    uint64_t Val = CI->getZExtValue();
    if (Width <= 8)
      appendBytes(Buffer, static_cast<uint8_t>(Val));
    else if (Width <= 16)
      appendBytes(Buffer, static_cast<uint16_t>(Val));
    else if (Width <= 32)
      appendBytes(Buffer, static_cast<uint32_t>(Val));
    else if (Width <= 64)
      appendBytes(Buffer, static_cast<uint64_t>(Val));
    else {
      WATEVER_UNIMPLEMENTED("handle constant int >64 bit");
    }
    return;
  }

  if (auto *CF = llvm::dyn_cast<llvm::ConstantFP>(C)) {
    if (CF->getType()->isDoubleTy()) {
      appendBytes(Buffer, CF->getValue().convertToDouble());
    } else if (CF->getType()->isFloatTy()) {
      appendBytes(Buffer, CF->getValue().convertToFloat());
    } else {
      WATEVER_UNIMPLEMENTED("handle floating point type {}",
                            llvmToString(*CF->getType()));
    }
    return;
  }

  if (llvm::isa<llvm::ConstantPointerNull>(C)) {
    Buffer.insert(Buffer.end(), DL.getTypeAllocSize(C->getType()), 0);
    return;
  }

  // zero initializers
  if (auto *CAZ = llvm::dyn_cast<llvm::ConstantAggregateZero>(C)) {
    uint64_t Size = DL.getTypeAllocSize(CAZ->getType());
    Buffer.insert(Buffer.end(), Size, 0);
    return;
  }

  if (auto *GV = llvm::dyn_cast<llvm::GlobalValue>(C)) {
    auto PtrSize = DL.getPointerSize();
    auto Offset = Buffer.size();
    // Reserve space for relocation
    Buffer.insert(Buffer.end(), PtrSize, 0);

    RelocationType RelocTy;
    if (llvm::isa<llvm::Function>(GV)) {
      RelocTy = (PtrSize == 8) ? RelocationType::R_WASM_TABLE_INDEX_I64
                               : RelocationType::R_WASM_TABLE_INDEX_I32;
    } else {
      RelocTy = (PtrSize == 8) ? RelocationType::R_WASM_MEMORY_ADDR_I64
                               : RelocationType::R_WASM_MEMORY_ADDR_I32;
    }
    // Symbol index gets patched later, when all GV have been defined
    auto Reloc = std::make_unique<RelocationEntry>(RelocTy, Offset, 0);
    auto *RelocPtr = Reloc.get();
    Relocs.push_back(std::move(Reloc));
    FixUps[RelocPtr] = GV;
    return;
  }

  if (auto *CE = llvm::dyn_cast<llvm::ConstantExpr>(C)) {
    if (auto *GEP = llvm::dyn_cast<llvm::GEPOperator>(CE)) {
      llvm::APInt Offset(DL.getPointerSizeInBits(), 0);
      if (GEP->accumulateConstantOffset(DL, Offset)) {
        if (auto *PtrConstant = llvm::dyn_cast<llvm::GlobalValue>(
                GEP->getPointerOperand()->stripPointerCasts())) {
          flattenConstant(PtrConstant, Buffer, Relocs, FixUps, DL);
          // Flatten constant will have added a new relocation for the GV, to
          // which this GEP is an addend
          Relocs.back()->Addend += Offset.getSExtValue();
          return;
        }
        WATEVER_UNIMPLEMENTED("GEP with non global value {} as operand",
                              GEP->getPointerOperand()->getNameOrAsOperand());
      }
      WATEVER_UNIMPLEMENTED("complex GEP {} in data section",
                            GEP->getNameOrAsOperand());
    }
    if (auto *PtrToInt = llvm::dyn_cast<llvm::PtrToIntOperator>(CE)) {
      if (auto *PtrConstant =
              llvm::dyn_cast<llvm::Constant>(PtrToInt->getPointerOperand())) {
        return flattenConstant(PtrConstant, Buffer, Relocs, FixUps, DL);
      }
      WATEVER_UNREACHABLE(
          "const expr PtrToInt has non const pointer as operand");
    }
    if (CE->getOpcode() == llvm::Instruction::IntToPtr) {
      return flattenConstant(CE->getOperand(0), Buffer, Relocs, FixUps, DL);
    }
  }

  WATEVER_UNIMPLEMENTED("constant data of type {} is not yet supported",
                        llvmToString(*C->getType()));
}

llvm::DenseMap<llvm::Value *, int>
BlockLowering::getDependencyTreeUserCount(llvm::Instruction *Root) const {
  llvm::DenseMap<llvm::Value *, int> Result;

  llvm::SmallVector<llvm::Value *> WorkList;
  llvm::SmallPtrSet<llvm::Value *, 16> ASTNodes;

  WorkList.push_back(Root);
  ASTNodes.insert(Root);

  // Store all nodes in the AST
  while (!WorkList.empty()) {
    auto *Val = WorkList.pop_back_val();
    if (auto *Inst = llvm::dyn_cast<llvm::Instruction>(Val)) {
      if (llvm::isa<llvm::PHINode>(Inst)) {
        continue;
      }
      // Stop at the AST leaf
      if (Root != Inst) {
        if (auto It = Parent.Roots.find(Root->getParent());
            It != Parent.Roots.end()) {
          if (It->second.contains(Inst)) {
            continue;
          }
        }
      }
      for (llvm::Value *Op : Inst->operands()) {
        if (auto *OpInst = llvm::dyn_cast<llvm::Instruction>(Op)) {
          if (OpInst->getParent() == Root->getParent()) {
            if (ASTNodes.insert(OpInst).second) {
              WorkList.push_back(OpInst);
            }
          }
        }
      }
    }
  }
  for (auto *Node : ASTNodes) {
    int Count = 0;
    for (auto *User : Node->users()) {
      if (ASTNodes.contains(User)) {
        Count++;
      }
    }
    Result[Node] = Count;
  }
  return Result;
}

void BlockLowering::handleIntrinsic(llvm::CallInst &CI) {
  switch (CI.getCalledFunction()->getIntrinsicID()) {
    // C/C++ Library Intrinsics
  case llvm::Intrinsic::memcpy:
  case llvm::Intrinsic::memmove: {
    assert(Parent.FeatureSet.bulk_memory_enabled() &&
           "cannot handle memcpy/memove without bulk_memory");

    // TODO if Len == 0 and any pointer invalid, WebAssembly traps, but it
    // should be a no-op
    auto *Dest = CI.getArgOperand(0);
    auto *Src = CI.getArgOperand(1);
    auto *Len = CI.getArgOperand(2);

    Actions.Insts.emplace_back(Opcode::MemoryCopy, 0, 0);
    WorkList.push_back(Dest);
    WorkList.push_back(Src);
    WorkList.push_back(Len);
    break;
  }
  case llvm::Intrinsic::memset: {
    assert(Parent.FeatureSet.bulk_memory_enabled() &&
           "cannot handle memset without bulk_memory");
    // TODO if Len == 0 and Dest invalid, WebAssembly traps, but it should be a
    // no-op. This should either be handled in legalization or we somehow need a
    // branch here.
    auto *Len = CI.getArgOperand(2);
    auto *Val = CI.getArgOperand(1);
    auto *Dest = CI.getArgOperand(0);

    Actions.Insts.emplace_back(Opcode::MemoryFill, int64_t{0});

    WorkList.push_back(Dest);
    // The argument might have been illegal, use the original i32
    if (auto *TruncInst = llvm::dyn_cast<llvm::TruncInst>(Val)) {
      WorkList.push_back(TruncInst->getOperand(0));
    } else if (llvm::isa<llvm::ConstantInt>(Val)) {
      WorkList.push_back(Val);
    } else {
      WATEVER_UNREACHABLE("fill memory with value {}",
                          Val->getNameOrAsOperand());
    }

    WorkList.push_back(Len);
    break;
  }
  case llvm::Intrinsic::sqrt: {
    auto *Ty = CI.getArgOperand(0)->getType();
    if (Ty->isFloatTy()) {
      Actions.Insts.emplace_back(Opcode::F32Sqrt);
    } else if (Ty->isDoubleTy()) {
      Actions.Insts.emplace_back(Opcode::F64Sqrt);
    } else {
      WATEVER_UNIMPLEMENTED("sqrt with type {}", llvmToString(*Ty));
    }
    WorkList.push_back(CI.getArgOperand(0));
    return;
  }
  case llvm::Intrinsic::floor: {
    auto *Ty = CI.getArgOperand(0)->getType();
    if (Ty->isFloatTy()) {
      Actions.Insts.emplace_back(Opcode::F32Floor);
    } else if (Ty->isDoubleTy()) {
      Actions.Insts.emplace_back(Opcode::F64Floor);
    } else {
      WATEVER_UNIMPLEMENTED("floor with type {}", llvmToString(*Ty));
    }
    WorkList.push_back(CI.getArgOperand(0));
    return;
  }
  case llvm::Intrinsic::ceil: {
    auto *Ty = CI.getArgOperand(0)->getType();
    if (Ty->isFloatTy()) {
      Actions.Insts.emplace_back(Opcode::F32Ceil);
    } else if (Ty->isDoubleTy()) {
      Actions.Insts.emplace_back(Opcode::F64Ceil);
    } else {
      WATEVER_UNIMPLEMENTED("floor with type {}", llvmToString(*Ty));
    }
    WorkList.push_back(CI.getArgOperand(0));
    return;
  }
  case llvm::Intrinsic::trunc: {
    auto *Ty = CI.getArgOperand(0)->getType();
    if (Ty->isFloatTy()) {
      Actions.Insts.emplace_back(Opcode::F32Trunc);
    } else if (Ty->isDoubleTy()) {
      Actions.Insts.emplace_back(Opcode::F64Trunc);
    } else {
      WATEVER_UNIMPLEMENTED("floor with type {}", llvmToString(*Ty));
    }
    WorkList.push_back(CI.getArgOperand(0));
    return;
  }
  // Bit Manipulation Intrinsics
  case llvm::Intrinsic::ctpop: {
    auto BitWidth = CI.getArgOperand(0)->getType()->getIntegerBitWidth();
    if (BitWidth == 32) {
      Actions.Insts.emplace_back(Opcode::I32Popcnt);
    } else if (BitWidth == 64) {
      Actions.Insts.emplace_back(Opcode::I64Popcnt);
    } else {
      WATEVER_UNIMPLEMENTED("Unsupported bit width {} for popcnt", BitWidth);
    }
    WorkList.push_back(CI.getArgOperand(0));
    return;
  }
  case llvm::Intrinsic::ctlz: {
    auto BitWidth = CI.getArgOperand(0)->getType()->getIntegerBitWidth();
    if (BitWidth == 32) {
      Actions.Insts.emplace_back(Opcode::I32Clz);
    } else if (BitWidth == 64) {
      Actions.Insts.emplace_back(Opcode::I64Clz);
    } else {
      WATEVER_UNIMPLEMENTED("Unsupported bit width {} for ctlz", BitWidth);
    }
    WorkList.push_back(CI.getArgOperand(0));
    return;
  }
  case llvm::Intrinsic::cttz: {
    auto BitWidth = CI.getArgOperand(0)->getType()->getIntegerBitWidth();
    if (BitWidth == 32) {
      Actions.Insts.emplace_back(Opcode::I32Ctz);
    } else if (BitWidth == 64) {
      Actions.Insts.emplace_back(Opcode::I64Ctz);
    } else {
      WATEVER_UNIMPLEMENTED("Unsupported bit width {} for cttz", BitWidth);
    }
    WorkList.push_back(CI.getArgOperand(0));
    return;
  }
  case llvm::Intrinsic::fshl: {
    if (CI.getArgOperand(0) != CI.getArgOperand(1)) {
      WATEVER_UNREACHABLE("backend can only handle rotl fshl");
    }
    auto BitWidth = CI.getArgOperand(0)->getType()->getIntegerBitWidth();
    if (BitWidth == 32) {
      Actions.Insts.emplace_back(Opcode::I32Rotl);
    } else if (BitWidth == 64) {
      Actions.Insts.emplace_back(Opcode::I64Rotl);
    }
    WorkList.push_back(CI.getArgOperand(0));
    WorkList.push_back(CI.getArgOperand(2));
    return;
  }
  case llvm::Intrinsic::fshr: {
    if (CI.getArgOperand(0) != CI.getArgOperand(1)) {
      WATEVER_UNREACHABLE("backend can only handle rotr fshr");
    }
    auto BitWidth = CI.getArgOperand(0)->getType()->getIntegerBitWidth();
    if (BitWidth == 32) {
      Actions.Insts.emplace_back(Opcode::I32Rotr);
    } else if (BitWidth == 64) {
      Actions.Insts.emplace_back(Opcode::I64Rotr);
    }
    WorkList.push_back(CI.getArgOperand(0));
    WorkList.push_back(CI.getArgOperand(2));
    return;
  }
  // General Intrinsics
  case llvm::Intrinsic::is_constant: {
    if (auto *C = llvm::dyn_cast<llvm::Constant>(CI.getArgOperand(0))) {
      if (C->isManifestConstant()) {
        Actions.Insts.emplace_back(Opcode::I32Const, int64_t{1});
        return;
      }
    }
    Actions.Insts.emplace_back(Opcode::I32Const, int64_t{0});
    return;
  }
  default: {
    WATEVER_TODO("handle {} intrinsic",
                 CI.getCalledFunction()->getName().str());
  }
  }
}

//===----------------------------------------------------------------------===//
// Unary Operations
//===----------------------------------------------------------------------===//
void BlockLowering::visitUnaryOperator(llvm::UnaryOperator &UO) {
  addOperandsToWorklist(UO.operands());

  switch (UO.getOpcode()) {
  case llvm::Instruction::FNeg: {
    if (UO.getType()->isDoubleTy()) {
      Actions.Insts.emplace_back(Opcode::F64Neg);
    } else if (UO.getType()->isFloatTy()) {
      Actions.Insts.emplace_back(Opcode::F32Neg);
    } else {
      WATEVER_UNREACHABLE("Illegal floating point for negation: {}",
                          llvmToString(*UO.getType()));
    }
    break;
  }
  default:
    WATEVER_UNREACHABLE("Illegal opcode encountered: {}", UO.getOpcodeName());
    break;
  }
}

//===----------------------------------------------------------------------===//
// Binary Operations
//===----------------------------------------------------------------------===//
void BlockLowering::visitBinaryOperator(llvm::BinaryOperator &BO) {
  const auto *Ty = BO.getType();
  // TODO handle vectors
  const unsigned Width = Ty->getPrimitiveSizeInBits();
  bool Handled = true;

  addOperandsToWorklist(BO.operands());

  auto Dispatch = [&](Opcode::Enum Op32, Opcode::Enum Op64) {
    if (Width == 32 || Width == 1)
      Actions.Insts.emplace_back(Op32);
    else if (Width == 64)
      Actions.Insts.emplace_back(Op64);
    else
      Handled = false;
  };

  switch (BO.getOpcode()) {
  case llvm::Instruction::Add: {
    Dispatch(Opcode::I32Add, Opcode::I64Add);
    break;
  }
  case llvm::Instruction::FAdd: {
    Dispatch(Opcode::F32Add, Opcode::F64Add);
    break;
  }
  case llvm::Instruction::Sub: {
    Dispatch(Opcode::I32Sub, Opcode::I64Sub);
    break;
  }
  case llvm::Instruction::FSub: {
    Dispatch(Opcode::F32Sub, Opcode::F64Sub);
    break;
  }
  case llvm::Instruction::Mul: {
    Dispatch(Opcode::I32Mul, Opcode::I64Mul);
    break;
  }
  case llvm::Instruction::FMul: {
    Dispatch(Opcode::F32Mul, Opcode::F64Mul);
    break;
  }
  case llvm::Instruction::UDiv: {
    Dispatch(Opcode::I32DivU, Opcode::I64DivU);
    break;
  }
  case llvm::Instruction::SDiv: {
    Dispatch(Opcode::I32DivS, Opcode::I64DivS);
    break;
  }
  case llvm::Instruction::FDiv: {
    Dispatch(Opcode::F32Div, Opcode::F64Div);
    break;
  }
  case llvm::Instruction::URem: {
    Dispatch(Opcode::I32RemU, Opcode::I64RemU);
    break;
  }
  case llvm::Instruction::SRem: {
    Dispatch(Opcode::I32RemS, Opcode::I64RemS);
    break;
  }
  case llvm::Instruction::FRem: {
    Handled = false;
    break;
  }
  case llvm::Instruction::Shl: {
    Dispatch(Opcode::I32Shl, Opcode::I64Shl);
    break;
  }
  case llvm::Instruction::LShr: {
    Dispatch(Opcode::I32ShrU, Opcode::I64ShrU);
    break;
  }
  case llvm::Instruction::AShr: {
    Dispatch(Opcode::I32ShrS, Opcode::I64ShrS);
    break;
  }
  case llvm::Instruction::And: {
    Dispatch(Opcode::I32And, Opcode::I64And);
    break;
  }
  case llvm::Instruction::Or: {
    Dispatch(Opcode::I32Or, Opcode::I64Or);
    break;
  }
  case llvm::Instruction::Xor: {
    Dispatch(Opcode::I32Xor, Opcode::I64Xor);
    break;
  }
  default:
    WATEVER_UNREACHABLE("Illegal opcode encountered: {}", BO.getOpcodeName());
    break;
  }
  if (!Handled) {
    WATEVER_TODO("lowering of {}-bit {}", Width, BO.getOpcodeName());
  }
}
//===----------------------------------------------------------------------===//
// Vector Operations
//===----------------------------------------------------------------------===//
//===----------------------------------------------------------------------===//
// Aggregate Operations
//===----------------------------------------------------------------------===//
//===----------------------------------------------------------------------===//
// Memory Access and Addressing Operations
//===----------------------------------------------------------------------===//

void BlockLowering::visitAllocaInst(llvm::AllocaInst &AI) {
  if (Parent.PromotedAllocas.contains(&AI)) {
    return;
  }

  const bool Is64Bit =
      AI.getModule()->getDataLayout().getPointerSizeInBits() == 64;
  const auto PtrTy = Is64Bit ? ValType::I64 : ValType::I32;
  const auto ConstOp = Is64Bit ? Opcode::I64Const : Opcode::I32Const;
  const auto AddOp = Is64Bit ? Opcode::I64Add : Opcode::I32Add;
  const auto AndOp = Is64Bit ? Opcode::I64And : Opcode::I32And;
  const auto SubOp = Is64Bit ? Opcode::I64Sub : Opcode::I32Sub;
  const auto MulOp = Is64Bit ? Opcode::I64Mul : Opcode::I32Mul;

  // Static allocation
  if (auto It = Parent.StackSlots.find(&AI); It != Parent.StackSlots.end()) {
    assert(Parent.FP.has_value() && "no frame pointer defined");
    // Offset has already been applied
    if (!AllocaSkipOffsetList.empty() &&
        AllocaSkipOffsetList.back() == WorkList.size()) {
      AllocaSkipOffsetList.pop_back();
    } else if (It->second != 0) {
      Actions.Insts.emplace_back(AddOp);
      Actions.Insts.emplace_back(ConstOp, static_cast<int64_t>(It->second));
    }
    Actions.Insts.emplace_back(Opcode::LocalGet,
                               std::make_unique<LocalArg>(Parent.FP.value()));
    return;
  }

  // Dynamic allocation
  auto *AllocatedType = AI.getAllocatedType();
  int64_t Size =
      AI.getModule()->getDataLayout().getTypeAllocSize(AllocatedType);

  ImportedGlobal *StackPointer;
  StackPointer = M.getStackPointer(PtrTy);

  if (Size == 0) {
    Actions.Insts.emplace_back(Opcode::GlobalGet, StackPointer);
    return;
  }

  // Note that the instructions are evaluated in reverse order

  // We need the modified stack pointer on top of the stack, there  is no
  // global tee
  Actions.Insts.emplace_back(Opcode::GlobalGet, StackPointer);
  Actions.Insts.emplace_back(Opcode::GlobalSet, StackPointer);

  Actions.Insts.emplace_back(SubOp);

  // Subtract the stack pointer
  // If it is in array, we need to multiply the size of one object with the
  // array size
  if (AI.isArrayAllocation()) {
    // TODO this fails if the type of the array size != ptr size
    /** TODO could avoid the extra local, if we could do
     * global.get __stack_pointer
     * <evaluate array size>
     * i32.const size
     * mul
     * <align>
     * sub
     *
     * Currently, the following is generated
     * <evaluate array size>
     * i32.const size
     * mul
     * <align>
     * local.set <total array size>
     * global.get __stack_pointer
     * local.get <total array size>
     * sub
     */
    uint32_t TotalSizeLocal = Parent.getNewLocal(PtrTy);
    Actions.Insts.emplace_back(Opcode::LocalGet,
                               std::make_unique<LocalArg>(TotalSizeLocal));
    Actions.Insts.emplace_back(Opcode::GlobalGet, StackPointer);
    Actions.Insts.emplace_back(Opcode::LocalSet,
                               std::make_unique<LocalArg>(TotalSizeLocal));
    // Align
    Actions.Insts.emplace_back(AndOp);
    Actions.Insts.emplace_back(ConstOp, int64_t{-16});
    Actions.Insts.emplace_back(AddOp);
    Actions.Insts.emplace_back(ConstOp, int64_t{15});
    // Calculate total size
    // TODO we probably can use a shl in most cases here
    if (Size != 1) {
      Actions.Insts.emplace_back(MulOp);
      Actions.Insts.emplace_back(ConstOp, Size);
    }
    WorkList.push_back(AI.getArraySize());
  } else {
    Size = llvm::alignTo(Size, 16);
    Actions.Insts.emplace_back(ConstOp, Size);
    Actions.Insts.emplace_back(Opcode::GlobalGet, StackPointer);
  }
}

void BlockLowering::doGreedyMemOp(llvm::Instruction &I, Opcode::Enum Op) {
  llvm::Value *Ptr = llvm::getLoadStorePointerOperand(&I);
  if (!Ptr) {
    WATEVER_UNREACHABLE("could not get pointer from memory operation");
  }

  // If we already have a local containing the pointer, we can just use our
  // pointer with offset instead of inlining offsets.
  if (Parent.LocalMapping.contains(Ptr)) {
    Actions.Insts.emplace_back(Op, std::make_unique<MemArg>());
    WorkList.push_back(Ptr);
    return;
  }

  if (auto *IntToPtr = llvm::dyn_cast<llvm::IntToPtrInst>(Ptr)) {
    if (auto *BinOp =
            llvm::dyn_cast<llvm::BinaryOperator>(IntToPtr->getOperand(0))) {
      // If we are the only user, and it's an addition, we can inline
      if (BinOp->getNumUses() <= 1 &&
          BinOp->getOpcode() == llvm::Instruction::Add) {
        if (auto *Offset =
                llvm::dyn_cast<llvm::ConstantInt>(BinOp->getOperand(1))) {

          // We cannot inline negative offsets
          if (Offset->getSExtValue() > 0) {
            WATEVER_LOG_TRACE("inlining offset");
            Actions.Insts.emplace_back(
                Op, std::make_unique<MemArg>(0, Offset->getZExtValue()));
            // We need the pointer (without the offset) on top of the stack
            WorkList.push_back(BinOp->getOperand(0));
            return;
          }
        }
      }
    }
  }

  // If using a static stack slot, inline it based of the FP
  if (auto *AI = llvm::dyn_cast<llvm::AllocaInst>(Ptr)) {
    // Check if this is a static slot
    if (Parent.StackSlots.contains(AI)) {
      Actions.Insts.emplace_back(
          Op, std::make_unique<MemArg>(0, Parent.StackSlots[AI]));
      AllocaSkipOffsetList.push_back(WorkList.size());
      WorkList.push_back(AI);
      return;
    }
  }

  Actions.Insts.emplace_back(Op, std::make_unique<MemArg>());
  WorkList.push_back(Ptr);
}

void BlockLowering::visitLoadInst(llvm::LoadInst &LI) {
  // Check, if we use a promoted alloca
  if (auto *Alloca = llvm::dyn_cast<llvm::AllocaInst>(LI.getPointerOperand())) {
    if (Parent.PromotedAllocas.contains(Alloca)) {
      auto Local = Parent.LocalMapping.lookup(Alloca);
      Actions.Insts.emplace_back(Opcode::LocalGet,
                                 std::make_unique<LocalArg>(Local));
      return;
    }
  }

  // Loads should happen in sext or zext, if they need to get extended.
  auto *LoadType = LI.getType();
  // uint32_t Alignment = LI.getAlign().value();

  // TODO use alignment
  if (LoadType->isIntegerTy()) {
    const unsigned Width = LoadType->getIntegerBitWidth();

    if (Width == 32) {
      doGreedyMemOp(LI, Opcode::I32Load);
      return;
    }
    if (Width == 64) {
      doGreedyMemOp(LI, Opcode::I64Load);
      return;
    }
    // i8 and i16 should be loaded over sext/zext
    WATEVER_UNREACHABLE("Cannot load integer of width {}", Width);
  }

  if (LoadType->isFloatingPointTy()) {
    if (LoadType->isDoubleTy()) {
      doGreedyMemOp(LI, Opcode::F64Load);
      return;
    }
    if (LoadType->isFloatTy()) {
      doGreedyMemOp(LI, Opcode::F32Load);
      return;
    }
    WATEVER_UNIMPLEMENTED("Unsupported floating point type {} for load",
                          llvmToString(*LoadType));
  }

  if (LoadType->isPointerTy()) {
    if (LI.getModule()->getDataLayout().getPointerSizeInBits() == 64) {
      doGreedyMemOp(LI, Opcode::I64Load);
    } else {
      doGreedyMemOp(LI, Opcode::I32Load);
    }
    return;
  }

  WATEVER_UNREACHABLE("Unsupported load type {}", llvmToString(*LoadType));
}

void BlockLowering::visitStoreInst(llvm::StoreInst &SI) {
  // Check, if we use a promoted alloca
  if (auto *Alloca = llvm::dyn_cast<llvm::AllocaInst>(SI.getPointerOperand())) {
    if (Parent.PromotedAllocas.contains(Alloca)) {
      auto Local = Parent.LocalMapping.lookup(Alloca);
      Actions.Insts.emplace_back(Opcode::LocalSet,
                                 std::make_unique<LocalArg>(Local));
      WorkList.push_back(SI.getValueOperand());
      return;
    }
  }

  auto *StoreType = SI.getOperand(0)->getType();

  if (StoreType->isIntegerTy()) {
    if (auto *TruncInst = llvm::dyn_cast<llvm::TruncInst>(SI.getOperand(0))) {
      auto FromWidth =
          TruncInst->getOperand(0)->getType()->getIntegerBitWidth();
      auto ToWidth = TruncInst->getType()->getIntegerBitWidth();

      if (FromWidth == 64) {
        if (ToWidth == 8) {
          doGreedyMemOp(SI, Opcode::I64Store8);
          WorkList.push_back(TruncInst->getOperand(0));
          return;
        }
        if (ToWidth == 16) {
          doGreedyMemOp(SI, Opcode::I64Store16);
          WorkList.push_back(TruncInst->getOperand(0));
          return;
        }
        if (ToWidth == 32) {
          doGreedyMemOp(SI, Opcode::I64Store32);
          WorkList.push_back(TruncInst->getOperand(0));
          return;
        }
      }
      if (FromWidth == 32) {
        if (ToWidth == 8) {
          doGreedyMemOp(SI, Opcode::I32Store8);
          WorkList.push_back(TruncInst->getOperand(0));
          return;
        }
        if (ToWidth == 16) {
          doGreedyMemOp(SI, Opcode::I32Store16);
          WorkList.push_back(TruncInst->getOperand(0));
          return;
        }
      }
    }

    const unsigned Width = StoreType->getIntegerBitWidth();
    // For constants, we allow 8 and 16-bit, as these are not legalized away.
    // However, we need to push it onto the worklist as a i32
    if (const auto *Const =
            llvm::dyn_cast<llvm::ConstantInt>(SI.getValueOperand())) {
      if (Width <= 8) {
        doGreedyMemOp(SI, Opcode::I32Store8);
      } else if (Width <= 16) {
        doGreedyMemOp(SI, Opcode::I32Store16);
      }
      if (Width <= 16) {
        WorkList.push_back(llvm::ConstantInt::get(
            llvm::Type::getInt32Ty(SI.getContext()), Const->getZExtValue()));
        return;
      }
    }
    if (Width == 32) {
      doGreedyMemOp(SI, Opcode::I32Store);
      WorkList.push_back(SI.getValueOperand());
      return;
    }
    if (Width == 64) {
      doGreedyMemOp(SI, Opcode::I64Store);
      WorkList.push_back(SI.getValueOperand());
      return;
    }
    WATEVER_UNREACHABLE("Cannot store integer of width {}", Width);
  }

  if (StoreType->isFloatingPointTy()) {
    if (StoreType->isDoubleTy()) {
      doGreedyMemOp(SI, Opcode::F64Store);
      WorkList.push_back(SI.getValueOperand());
      return;
    }
    if (StoreType->isFloatTy()) {
      doGreedyMemOp(SI, Opcode::F32Store);
      WorkList.push_back(SI.getValueOperand());
      return;
    }
    WATEVER_UNIMPLEMENTED("Unsupported floating point type {} for store",
                          llvmToString(*StoreType));
  }

  if (StoreType->isPointerTy()) {
    if (SI.getModule()->getDataLayout().getPointerSizeInBits() == 64) {
      doGreedyMemOp(SI, Opcode::I64Store);
    } else {
      doGreedyMemOp(SI, Opcode::I32Store);
    }
    WorkList.push_back(SI.getValueOperand());
    return;
  }
  WATEVER_UNREACHABLE("Unsupported store type {}", llvmToString(*StoreType));
}

//===----------------------------------------------------------------------===//
// Conversion Operations
//===----------------------------------------------------------------------===//
void BlockLowering::visitTruncInst(llvm::TruncInst &TI) {
  auto FromWidth = TI.getOperand(0)->getType()->getIntegerBitWidth();
  auto ToWidth = TI.getType()->getIntegerBitWidth();

  WorkList.push_back(TI.getOperand(0));
  if (FromWidth > 32 && ToWidth <= 32) {
    Actions.Insts.push_back(Opcode::I32WrapI64);
    return;
  }

  // Otherwise, truncation is a no-op
}

void BlockLowering::visitZExtInst(llvm::ZExtInst &ZI) {
  auto FromWidth = ZI.getOperand(0)->getType()->getIntegerBitWidth();
  auto ToWidth = ZI.getType()->getIntegerBitWidth();

  if (auto *LI = llvm::dyn_cast<llvm::LoadInst>(ZI.getOperand(0))) {
    // We have a promoted alloca, so do not try to inline offsets/force a
    // load
    bool IsPromoted = false;
    if (auto *AI = llvm::dyn_cast<llvm::AllocaInst>(LI->getPointerOperand())) {
      IsPromoted = Parent.PromotedAllocas.contains(AI);
    }
    // TODO use alignment
    // uint32_t Alignment = LoadInst->getAlign().value();
    if (!IsPromoted) {
      if (ToWidth == 32) {
        if (FromWidth == 8) {
          doGreedyMemOp(*LI, Opcode::I32Load8U);
          return;
        }
        if (FromWidth == 16) {
          doGreedyMemOp(*LI, Opcode::I32Load16U);
          return;
        }
      }
      if (ToWidth == 64) {
        if (FromWidth == 8) {
          doGreedyMemOp(*LI, Opcode::I64Load8U);
          return;
        }
        if (FromWidth == 16) {
          doGreedyMemOp(*LI, Opcode::I64Load16U);
          return;
        }
        if (FromWidth == 32) {
          doGreedyMemOp(*LI, Opcode::I64Load32U);
          return;
        }
      }
      WATEVER_UNREACHABLE("unknown load from {} to {} bit", FromWidth, ToWidth);
    }
  }

  if (FromWidth <= 32 && ToWidth > 32 && ToWidth <= 64) {
    Actions.Insts.emplace_back(Opcode::I64ExtendI32U);
  }
  WorkList.push_back(ZI.getOperand(0));
}

void BlockLowering::visitSExtInst(llvm::SExtInst &SI) {
  auto FromWidth = SI.getOperand(0)->getType()->getIntegerBitWidth();
  auto ToWidth = SI.getType()->getIntegerBitWidth();

  if (FromWidth == 32 && ToWidth == 64) {
    addOperandsToWorklist(SI.operands());
    Actions.Insts.emplace_back(Opcode::I64ExtendI32S);
    return;
  }

  if (M.Config.EnabledFeatures.sign_ext_enabled()) {
    if (auto *TruncInst = llvm::dyn_cast<llvm::TruncInst>(SI.getOperand(0))) {
      addOperandsToWorklist(TruncInst->operands());
      auto FromTrunc = TruncInst->getSrcTy()->getIntegerBitWidth();

      // We want to expand a value of type FromTrunc with width FromWidth to
      // ToWidth.
      if (ToWidth == 64) {
        if (FromWidth == 8) {
          Actions.Insts.emplace_back(Opcode::I64Extend8S);
        } else if (FromWidth == 16) {
          Actions.Insts.emplace_back(Opcode::I64Extend16S);
        } else if (FromWidth == 32) {
          // Needed if we want to extend an I64 with only the lower 32-bits
          // filled to a full I64. (This is not handled by default - note the
          // missing I)
          Actions.Insts.emplace_back(Opcode::I64Extend32S);
        } else {
          WATEVER_UNREACHABLE("illegal from width {} in sign-ext", FromWidth);
        }

        // However, if the origianl Wasm type was only 32-bit, we first need
        // to extend that type on the stack.
        if (FromTrunc == 32) {
          Actions.Insts.emplace_back(Opcode::I64ExtendI32U);
        } else if (FromTrunc != 64) {
          WATEVER_UNREACHABLE("sign-ext to 64 on illegal type {}", FromTrunc);
        }
        return;
      }
      if (ToWidth == 32) {
        assert(FromTrunc == 32 &&
               "sign-ext to 32 can only be done on a 32-bit type");

        if (FromWidth == 8) {
          Actions.Insts.emplace_back(Opcode::I32Extend8S);
        } else if (FromWidth == 16) {
          Actions.Insts.emplace_back(Opcode::I32Extend16S);
        } else {
          WATEVER_UNREACHABLE("illegal from width {} in sign-ext", FromWidth);
        }
        return;
      }

    } else {
      WATEVER_UNREACHABLE("sign-ext instructions expect a previous trunc");
    }
  }

  WATEVER_UNREACHABLE("Can not expand from {} to {}", FromWidth, ToWidth);
}

void BlockLowering::visitFPTruncInst(llvm::FPTruncInst &FI) {
  addOperandsToWorklist(FI.operands());
  if (FI.getSrcTy()->isDoubleTy() && FI.getDestTy()->isFloatTy()) {
    Actions.Insts.emplace_back(Opcode::F32DemoteF64);
    return;
  }
  WATEVER_UNREACHABLE("Can not truncate from {} to {}",
                      llvmToString(*FI.getSrcTy()),
                      llvmToString(*FI.getDestTy()));
}

void BlockLowering::visitFPExtInst(llvm::FPExtInst &FI) {
  addOperandsToWorklist(FI.operands());
  if (FI.getSrcTy()->isFloatTy() && FI.getDestTy()->isDoubleTy()) {
    Actions.Insts.emplace_back(Opcode::F64PromoteF32);
    return;
  }
  WATEVER_UNREACHABLE("Can not extend from {} to {}",
                      llvmToString(*FI.getSrcTy()),
                      llvmToString(*FI.getDestTy()));
}

void BlockLowering::visitFPToUIInst(llvm::FPToUIInst &FI) {
  addOperandsToWorklist(FI.operands());
  auto ToWidth = FI.getDestTy()->getIntegerBitWidth();

  if (FI.getSrcTy()->isFloatTy()) {
    if (ToWidth == 32) {
      Actions.Insts.emplace_back(Opcode::I32TruncSatF32U);
      return;
    }
    if (ToWidth == 64) {
      Actions.Insts.emplace_back(Opcode::I64TruncSatF32U);
      return;
    }
  } else if (FI.getSrcTy()->isDoubleTy()) {
    if (ToWidth == 32) {
      Actions.Insts.emplace_back(Opcode::I32TruncSatF64U);
      return;
    }
    if (ToWidth == 64) {
      Actions.Insts.emplace_back(Opcode::I64TruncSatF64U);
      return;
    }
  }

  WATEVER_UNREACHABLE("Can not FP to UI from {} to {}",
                      llvmToString(*FI.getSrcTy()),
                      llvmToString(*FI.getDestTy()));
}

void BlockLowering::visitFPToSIInst(llvm::FPToSIInst &FI) {
  addOperandsToWorklist(FI.operands());
  auto ToWidth = FI.getDestTy()->getIntegerBitWidth();

  if (FI.getSrcTy()->isFloatTy()) {
    if (ToWidth == 32) {
      Actions.Insts.emplace_back(Opcode::I32TruncSatF32S);
      return;
    }
    if (ToWidth == 64) {
      Actions.Insts.emplace_back(Opcode::I64TruncSatF32S);
      return;
    }
  } else if (FI.getSrcTy()->isDoubleTy()) {
    if (ToWidth == 32) {
      Actions.Insts.emplace_back(Opcode::I32TruncSatF64S);
      return;
    }
    if (ToWidth == 64) {
      Actions.Insts.emplace_back(Opcode::I64TruncSatF64S);
      return;
    }
  }

  WATEVER_UNREACHABLE("Can not FP to SI from {} to {}",
                      llvmToString(*FI.getSrcTy()),
                      llvmToString(*FI.getDestTy()));
}

void BlockLowering::visitUIToFPInst(llvm::UIToFPInst &UI) {
  auto FromWidth = UI.getSrcTy()->getIntegerBitWidth();

  addOperandsToWorklist(UI.operands());

  if (UI.getDestTy()->isFloatTy()) {
    if (FromWidth == 32) {
      Actions.Insts.emplace_back(Opcode::F32ConvertI32U);
    } else {
      Actions.Insts.emplace_back(Opcode::F32ConvertI64U);
    }
  } else if (UI.getDestTy()->isDoubleTy()) {
    if (FromWidth == 32) {
      Actions.Insts.emplace_back(Opcode::F64ConvertI32U);
    } else {
      Actions.Insts.emplace_back(Opcode::F64ConvertI64U);
    }
  } else {
    WATEVER_UNREACHABLE("Can not UI to FP for {}",
                        llvmToString(*UI.getDestTy()));
  }
}

void BlockLowering::visitSIToFPInst(llvm::SIToFPInst &SI) {
  auto FromWidth = SI.getSrcTy()->getIntegerBitWidth();

  addOperandsToWorklist(SI.operands());

  if (SI.getDestTy()->isFloatTy()) {
    if (FromWidth == 32) {
      Actions.Insts.emplace_back(Opcode::F32ConvertI32S);
    } else {
      Actions.Insts.emplace_back(Opcode::F32ConvertI64S);
    }
  } else if (SI.getDestTy()->isDoubleTy()) {
    if (FromWidth == 32) {
      Actions.Insts.emplace_back(Opcode::F64ConvertI32S);
    } else {
      Actions.Insts.emplace_back(Opcode::F64ConvertI64S);
    }
  } else {
    WATEVER_UNREACHABLE("Can not SI to FP for {}",
                        llvmToString(*SI.getDestTy()));
  }
}

void BlockLowering::visitBitCastInst(llvm::BitCastInst &BI) {
  auto *SrcTy = BI.getSrcTy();
  auto *DestTy = BI.getDestTy();

  addOperandsToWorklist(BI.operands());
  // No-op
  if (SrcTy == DestTy) {
    return;
  }

  if (SrcTy->isFloatTy()) {
    if (DestTy->isIntegerTy(32)) {
      Actions.Insts.emplace_back(Opcode::I32ReinterpretF32);
      return;
    }
  }

  if (SrcTy->isIntegerTy(32)) {
    if (DestTy->isFloatTy()) {
      Actions.Insts.emplace_back(Opcode::F32ReinterpretI32);
      return;
    }
  }

  if (SrcTy->isDoubleTy()) {
    if (DestTy->isIntegerTy(64)) {
      Actions.Insts.emplace_back(Opcode::I64ReinterpretF64);
      return;
    }
  }

  if (SrcTy->isIntegerTy(64)) {
    if (DestTy->isDoubleTy()) {
      Actions.Insts.emplace_back(Opcode::F64ReinterpretI64);
      return;
    }
  }

  WATEVER_UNREACHABLE("Can not bitcast from {} to {}", llvmToString(*SrcTy),
                      llvmToString(*DestTy));
}

//===----------------------------------------------------------------------===//
// Other Operations
//===----------------------------------------------------------------------===//
void BlockLowering::visitICmpInst(llvm::ICmpInst &II) {
  unsigned Width;

  if (II.getOperand(0)->getType()->isPointerTy()) {
    Width = II.getModule()->getDataLayout().getPointerSizeInBits();
  } else {
    Width = II.getOperand(0)->getType()->getIntegerBitWidth();
  }

  bool Handled = true;

  addOperandsToWorklist(II.operands());

  auto Dispatch = [&](Opcode::Enum Op32, Opcode::Enum Op64) {
    if (Width == 32)
      Actions.Insts.emplace_back(Op32);
    else if (Width == 64)
      Actions.Insts.emplace_back(Op64);
    else
      Handled = false;
  };

  switch (II.getCmpPredicate()) {
  case llvm::CmpInst::ICMP_EQ: {
    Dispatch(Opcode::I32Eq, Opcode::I64Eq);
    break;
  }
  case llvm::CmpInst::ICMP_NE: {
    Dispatch(Opcode::I32Ne, Opcode::I64Ne);
    break;
  }
  case llvm::CmpInst::ICMP_UGT: {
    Dispatch(Opcode::I32GtU, Opcode::I64GtU);
    break;
  }
  case llvm::CmpInst::ICMP_UGE: {
    Dispatch(Opcode::I32GeU, Opcode::I64GeU);
    break;
  }
  case llvm::CmpInst::ICMP_ULT: {
    Dispatch(Opcode::I32LtU, Opcode::I64LtU);
    break;
  }
  case llvm::CmpInst::ICMP_ULE: {
    Dispatch(Opcode::I32LeU, Opcode::I64LeU);
    break;
  }
  case llvm::CmpInst::ICMP_SGT: {
    Dispatch(Opcode::I32GtS, Opcode::I64GtS);
    break;
  }
  case llvm::CmpInst::ICMP_SGE: {
    Dispatch(Opcode::I32GeS, Opcode::I64GeS);
    break;
  }
  case llvm::CmpInst::ICMP_SLT: {
    Dispatch(Opcode::I32LtS, Opcode::I64LtS);
    break;
  }
  case llvm::CmpInst::ICMP_SLE: {
    Dispatch(Opcode::I32LeS, Opcode::I64LeS);
    break;
  }
  default:
    WATEVER_UNREACHABLE("Illegal int comparison");
  }

  if (!Handled) {
    WATEVER_UNREACHABLE("Unhandled ICMP");
  }
}

void BlockLowering::visitFCmpInst(llvm::FCmpInst &FI) {
  const unsigned Width = FI.getOperand(0)->getType()->getScalarSizeInBits();
  bool Handled = true;

  auto Pred = FI.getPredicate();

  if (Pred != llvm::CmpInst::FCMP_TRUE && Pred != llvm::CmpInst::FCMP_FALSE) {
    addOperandsToWorklist(FI.operands());
  }

  auto Dispatch = [&](Opcode::Enum Op32, Opcode::Enum Op64) {
    if (Width == 32)
      Actions.Insts.emplace_back(Op32);
    else if (Width == 64)
      Actions.Insts.emplace_back(Op64);
    else
      Handled = false;
  };

  switch (FI.getPredicate()) {
  case llvm::CmpInst::FCMP_FALSE: {
    Actions.Insts.emplace_back(Opcode::I32Const, int64_t{0});
    break;
  }
  case llvm::CmpInst::FCMP_OEQ: {
    Dispatch(Opcode::F32Eq, Opcode::F64Eq);
    break;
  }
  case llvm::CmpInst::FCMP_OGT: {
    Dispatch(Opcode::F32Gt, Opcode::F64Gt);
    break;
  }
  case llvm::CmpInst::FCMP_OGE: {
    Dispatch(Opcode::F32Ge, Opcode::F64Ge);
    break;
  }
  case llvm::CmpInst::FCMP_OLT: {
    Dispatch(Opcode::F32Lt, Opcode::F64Lt);
    break;
  }
  case llvm::CmpInst::FCMP_OLE: {
    Dispatch(Opcode::F32Le, Opcode::F64Le);
    break;
  }
  case llvm::CmpInst::FCMP_UNE: {
    Dispatch(Opcode::F32Ne, Opcode::F64Ne);
    break;
  }
  case llvm::CmpInst::FCMP_TRUE: {
    Actions.Insts.emplace_back(Opcode::I32Const, int64_t{1});
    break;
  }
  case llvm::CmpInst::FCMP_ORD: {
    WATEVER_UNREACHABLE("Ordered check is not supported");
  }
  case llvm::CmpInst::FCMP_UNO:
  case llvm::CmpInst::FCMP_UEQ:
  case llvm::CmpInst::FCMP_UGT:
  case llvm::CmpInst::FCMP_UGE:
  case llvm::CmpInst::FCMP_ULT:
  case llvm::CmpInst::FCMP_ULE:
  case llvm::CmpInst::FCMP_ONE:
    WATEVER_UNREACHABLE("Unordered float comparisons are not supported");
  default:
    WATEVER_UNREACHABLE("Illegal float comparison");
  }
}

void BlockLowering::visitPHINode(llvm::PHINode &PN) {
  Actions.Insts.emplace_back(
      Opcode::LocalGet, std::make_unique<LocalArg>(Parent.LocalMapping[&PN]));
}

void BlockLowering::visitSelectInst(llvm::SelectInst &SI) {
  Actions.Insts.push_back(Opcode::Select);
  WorkList.push_back(SI.getTrueValue());
  WorkList.push_back(SI.getFalseValue());
  WorkList.push_back(SI.getCondition());
}

void BlockLowering::visitCallInst(llvm::CallInst &CI) {
  auto *Callee = CI.getCalledFunction();
  if (Callee && Callee->isIntrinsic()) {
    return handleIntrinsic(CI);
  }
  addOperandsToWorklist(CI.args());

  if (Callee) {
    auto *Func = M.FunctionMap[Callee];
    Actions.Insts.emplace_back(Opcode::Call,
                               std::make_unique<RelocatableFuncArg>(Func));
  } else {
    WorkList.push_back(CI.getCalledOperand());
    auto *FuncTable = M.getIndirectFunctionTable();
    auto *FT = CI.getFunctionType();
    FuncType WasmFuncTy{};
    for (auto *ParamTy : FT->params()) {
      auto WasmType = fromLLVMType(ParamTy, CI.getModule()->getDataLayout());
      WasmFuncTy.Params.push_back(WasmType);
    }

    if (!FT->getReturnType()->isVoidTy()) {
      WasmFuncTy.Results.push_back(
          fromLLVMType(FT->getReturnType(), CI.getModule()->getDataLayout()));
    }

    const uint32_t FuncTypeIndex = M.getOrAddType(WasmFuncTy);

    Actions.Insts.emplace_back(
        Opcode::CallIndirect,
        std::make_unique<RelocatableIndirectCallArg>(FuncTypeIndex, FuncTable));
  }
}

void BlockLowering::lower() {
  WATEVER_LOG_TRACE("Lowering {}", getBlockName(BB));

  const auto &Roots = Parent.Roots.lookup(BB);

  std::optional<uint32_t> LastSetRoot = std::nullopt;
  bool RootOnlyOneUse = false;

  for (auto *Root : Roots) {
    // For each instructions with possible side effects, build a dependency
    // tree on the stack in reverse order.
    WATEVER_LOG_TRACE("{} is AST root, materialize", llvmToString(*Root));

    WorkList.push_back(Root);
    auto Counts = getDependencyTreeUserCount(Root);

    while (!WorkList.empty()) {
      auto *Next = WorkList.pop_back_val();
      WATEVER_LOG_TRACE("handling {}", llvmToString(*Next));

      bool IsGreedyOptimization =
          !AllocaSkipOffsetList.empty() &&
          AllocaSkipOffsetList.back() == WorkList.size();

      // If this instruction is using a stack slot, we don't want to get it
      // from a potential local as we have already inlined the offset into
      // the store/load
      if (IsGreedyOptimization) {
        if (auto *Inst = llvm::dyn_cast<llvm::AllocaInst>(Next)) {
          visit(*Inst);
          continue;
        }
        WATEVER_UNREACHABLE("trying to inline memory offset on non-alloca");
      }

      // Check if an earlier instrucion has already produced this value
      // outside this AST (e.g., we might be the second user)
      if (auto It = Parent.LocalMapping.find(Next);
          It != Parent.LocalMapping.end()) {
        auto *Inst = llvm::dyn_cast<llvm::Instruction>(Next);
        // Only use the local if it comes from another BB, or has been
        // emitted in this BB in an earler AST.
        if (!Inst || llvm::isa<llvm::PHINode>(Inst) ||
            Inst->getParent() != BB || Emitted.contains(Inst)) {
          Actions.Insts.emplace_back(Opcode::LocalGet,
                                     std::make_unique<LocalArg>(It->second));
          WATEVER_LOG_TRACE("has already a local, loading");
          continue;
        }
      }

      // There will come an instruction in the worklist materializing Next
      if (Counts[Next] > 1) {
        WATEVER_LOG_TRACE("will get materialized later");
        Counts[Next]--;
        // Create a local or get one from the colorer
        auto L = Parent.getOrCreateLocal(Next, BB->getDataLayout());
        Actions.Insts.emplace_back(Opcode::LocalGet,
                                   std::make_unique<LocalArg>(L));
        continue;
      }

      // Next needs to be materialized
      if (auto *Inst = llvm::dyn_cast<llvm::Instruction>(Next)) {
        // Save to local, if we have a later user in this tree or this
        // block, who expects to be able to load the value.
        // TODO this might lead to unnecessary emissions:
        // - If Inst is AllocaInst, and is otherwise always inlined
        // (IsGreedyOptimization)
        if (Inst->getNumUses() > 1 && Inst != Root) {
          // Creation is needed, if Next is not used outside this block, but
          // in a later AST.
          uint32_t L = Parent.getOrCreateLocal(Next, BB->getDataLayout());
          Actions.Insts.emplace_back(Opcode::LocalTee,
                                     std::make_unique<LocalArg>(L));
          WATEVER_LOG_TRACE("has multiple users so we tee it");
        }
        visit(*Inst);
        Emitted.insert(Inst);
      } else if (auto *Arg = llvm::dyn_cast<llvm::Instruction>(Next)) {
        Actions.Insts.emplace_back(
            Opcode::LocalGet,
            std::make_unique<LocalArg>(Parent.LocalMapping.lookup(Arg)));
      } else if (!putValueOnStack(Next, Actions, M,
                                  BB->getDataLayout().getPointerSizeInBits() ==
                                      64)) {
        WATEVER_UNIMPLEMENTED("put {} on top of the stack",
                              llvmToString(*Next));
      }
    }
    assert(AllocaSkipOffsetList.empty() &&
           "empty worklist expects no remaining no-offset alloca");

    // Some intrinsics or branches might not generate any instructions
    if (Actions.Insts.empty()) {
      continue;
    }
    // Optimize away local.set local.get
    if (Actions.Insts.back().Op == Opcode::LocalGet) {
      if (const auto *Arg = llvm::dyn_cast_or_null<LocalArg>(
              Actions.Insts.back().getArgument())) {
        if (Arg->Index == LastSetRoot) {
          WATEVER_LOG_TRACE("Optimizing local.set, local.get sequence for {}",
                            LastSetRoot.value());
          Actions.Insts.pop_back(); // Remove get
          assert(Parent.Body.Insts.back().Op == Opcode::LocalSet &&
                 "old root did not set its root");
          Parent.Body.Insts.pop_back(); // Remove set
          // If others need this value, the local still has to be set
          bool needsTee = !RootOnlyOneUse;
          // The original root might have been a PHI node, and is just a get.
          // In that case, the tee is not needed anyway.
          if (!Parent.Body.Insts.empty() &&
              Parent.Body.Insts.back().Op == Opcode::LocalGet) {
            if (const auto *Arg = llvm::dyn_cast_or_null<LocalArg>(
                    Parent.Body.Insts.back().getArgument())) {
              if (Arg->Index == LastSetRoot) {
                needsTee = false;
              }
            }
          }

          if (needsTee) {
            Parent.Body.Insts.emplace_back(
                Opcode::LocalTee,
                std::make_unique<LocalArg>(LastSetRoot.value()));
          }
        }
      }
    }
    std::ranges::reverse(Actions.Insts);
    Parent.Body.Insts.insert(Parent.Body.Insts.end(),
                             std::make_move_iterator(Actions.Insts.begin()),
                             std::make_move_iterator(Actions.Insts.end()));
    Actions.Insts.clear();

    if (auto It = Parent.LocalMapping.find(Root);
        It != Parent.LocalMapping.end()) {
      // Mark the just-set value, so the next tree can reuse it if needed
      LastSetRoot = It->second;
      // The local can only be omitted, if there is only one user. As pointers
      // might have a single offset-calculating user, which is always inlined
      // and then potentially used multiple times, it is generally not safe to
      // omit locals with pointers.
      // See regression 260130.
      auto CanSkipLocal = [&]() {
        // If there are multiple users, a local is needed, so every user
        // can access it
        if (Root->getNumUses() > 1) {
          return false;
        }

        // Only pointers might get inlined users, so if not a pointer, omit
        // setting the local is safe
        if (!Root->getType()->isPointerTy()) {
          return true;
        }
        // If this is an offset-adding instruction, check if the final pointer
        // generated with inttoptr has only a single use
        if (llvm::User *User = llvm::dyn_cast<llvm::PtrToIntInst>(
                Root->use_begin()->getUser())) {
          // Only check the dependency tree 5 uses up
          for (uint32_t I = 0; I < 10; ++I) {
            if (User->getNumUses() > 1) {
              // If there are multiple users, IntToPtr will not have a local, as
              // it is always inlined, so this root needs to be
              if (llvm::isa<llvm::IntToPtrInst>(User)) {
                return false;
              }
              return true;
            }
            if (User->user_empty()) {
              return true;
            }
            User = User->use_begin()->getUser();
          }
        }
        return true;
      };
      RootOnlyOneUse = CanSkipLocal();

      Parent.Body.Insts.emplace_back(Opcode::LocalSet,
                                     std::make_unique<LocalArg>(It->second));
    } else {
      LastSetRoot = std::nullopt;
      if (Root->getNumUses() > 0) {
        WATEVER_UNREACHABLE(
            "Multi-use instruction {} did not have a local assigned to it",
            Root->getNameOrAsOperand());
      }
      if (!Root->getType()->isVoidTy()) {
        Parent.Body.Insts.emplace_back(Opcode::Drop);
      }
    }
  }
}

uint32_t FunctionLowering::index(const llvm::BasicBlock *BB,
                                 const Context &Ctx) {
  uint32_t I = 0;
  // Iterate in reverse to find the innermost matching label (relative index
  // 0)
  for (auto &It : std::ranges::reverse_view(Ctx.Enclosing)) {
    if (It == BB) {
      return I;
    }
    ++I;
  }
  WATEVER_UNREACHABLE("unknown branch target");
}

void FunctionLowering::processTree(llvm::BasicBlock *Root, ValType FTy,
                                   llvm::BasicBlock *Fallthrough) {
  WATEVER_LOG_TRACE("Processing tree rooted at {}, producing {}",
                    getBlockName(Root), toString(FTy));

  llvm::SmallVector<llvm::BasicBlock *> MergeChildren;
  getMergeChildren(Root, MergeChildren);

  // If there is a fall through, this tree can just produce void. Otherwise, it
  // produces the function result.
  auto TreeTy = Fallthrough == nullptr ? FTy : ValType::Void;

  if (LI.isLoopHeader(Root)) {
    WATEVER_LOG_TRACE("Generating loop for {}", getBlockName(Root));
    Ctx.Enclosing.push_back(Root);
    F.Body.Insts.push_back(WasmInst::createLoop(TreeTy));
    WorkStack.push_back(PopContext());
    WorkStack.push_back(EmitOp{.Op = Opcode::End});
  }

  llvm::BasicBlock *NextFallthrough = Fallthrough;

  bool GeneratesIf = false;
  if (auto *Br = llvm::dyn_cast<llvm::BranchInst>(Root->getTerminator())) {
    GeneratesIf = Br->isConditional();
  }

  // Remember to process all merge children: they need to be reachable from
  // Root, so everyone gets a block. The first child is the outermost merge.
  for (auto *Child : MergeChildren) {
    // If the child is the current follower, no block is needed. The caller (or
    // the previous loop iteration) has already provided the scope that targets
    // this child.
    bool IsFollower = Child == NextFallthrough;
    bool IsInnermost = (Child == MergeChildren.back());

    bool SkipBlock = IsFollower || (IsInnermost && GeneratesIf);

    if (!SkipBlock) {
      F.Body.Insts.push_back(WasmInst::createBlock(ValType::Void));
      Ctx.Enclosing.push_back(Child);
    }

    // Schedule the child
    WorkStack.push_back(ProcessTree(Child, FTy, NextFallthrough));

    if (!SkipBlock) {
      WorkStack.push_back(EmitOp{.Op = Opcode::End});
      WorkStack.push_back(PopContext());
    }

    NextFallthrough = Child;
  }

  translateBB(Root);

  auto *Term = Root->getTerminator();

  if (auto *Br = llvm::dyn_cast<llvm::BranchInst>(Term)) {
    auto RootTy = MergeChildren.empty() ? TreeTy : ValType::Void;
    if (Br->isConditional()) {
      WATEVER_LOG_TRACE("{} branches to {} and {}", getBlockName(Root),
                        getBlockName(Br->getSuccessor(0)),
                        getBlockName(Br->getSuccessor(1)));

      // TODO In the following case, a br_if might be prefered:
      //    A
      //   / \.
      //  B-->C
      //
      //  Goal           Current
      // ------         ---------
      // block          A
      //   A            if
      //   br_if        else
      //   B              B
      // end            end
      // C              C
      //
      // In theory one could check, if one of the successors is follower:
      // we could satisfy the branch with a br_if. However, this will
      // never be the case, as the edge from A to C is critical and
      // therefore always split (what ends up in the empty if-case). And
      // at this point, it cannot be decided, whether the PHI nodes in C
      // needs a move in the critical-edge-block or not.
      //
      // if-else statements are basically always shorter, but nest the
      // control flow.

      Ctx.Enclosing.push_back(NextFallthrough);
      F.Body.Insts.push_back(WasmInst::createIfElse(RootTy));
      WorkStack.push_back(PopContext());
      WorkStack.push_back(EmitOp{.Op = Opcode::End});
      // Else path
      WorkStack.push_back(
          HandleEdge(Root, Br->getSuccessor(1), RootTy, NextFallthrough));
      WorkStack.push_back(EmitOp{.Op = Opcode::Else});
      // If path
      WorkStack.push_back(
          HandleEdge(Root, Br->getSuccessor(0), RootTy, NextFallthrough));
    } else {
      WATEVER_LOG_TRACE("{} branches to {}", getBlockName(Root),
                        getBlockName(Br->getSuccessor(0)));
      WorkStack.push_back(
          HandleEdge(Root, Br->getSuccessor(0), RootTy, NextFallthrough));
    }
  } else if (llvm::isa<llvm::ReturnInst>(Term)) {
    F.Body.Insts.emplace_back(Opcode::Return);
  } else if (llvm::isa<llvm::UnreachableInst>(Term)) {
    F.Body.Insts.emplace_back(Opcode::Unreachable);
  } else if (auto *SI = llvm::dyn_cast<llvm::SwitchInst>(Term)) {
    WATEVER_LOG_TRACE("{} ends with a switch", getBlockName(Root));
    auto *DefaultTarget = SI->getDefaultDest();
    auto DefaultIdx = index(DefaultTarget, Ctx);

    // Map each case to the target block index
    // These have been ordered during legalization
    llvm::SmallVector<uint32_t> Targets;
    [[maybe_unused]] uint32_t LastCase = 0;
    for (auto &Case : SI->cases()) {
      auto *Target = Case.getCaseSuccessor();
      auto BlockIdx = index(Target, Ctx);
      auto CaseValue = Case.getCaseValue()->getZExtValue();

      // Fill gaps
      while (Targets.size() < CaseValue) {
        Targets.push_back(DefaultIdx);
      }
      assert(LastCase <= CaseValue && "cases have not been ordered correctly");
      LastCase = CaseValue;
      Targets.push_back(BlockIdx);
    }
    BranchTableArg Argument{std::move(Targets), DefaultIdx};
    F.Body.Insts.emplace_back(Opcode::BrTable,
                              std::make_unique<BranchTableArg>(Argument));
  } else {
    WATEVER_UNREACHABLE("unsupported terminator: {}", Term->getOpcodeName());
  }
}

void FunctionLowering::handleEdge(llvm::BasicBlock *Source,
                                  llvm::BasicBlock *Target, ValType Ty,
                                  llvm::BasicBlock *Fallthrough) {
  // Actions to be executed on the edge
  llvm::SmallVector<uint32_t> Destinations;

  // Put all phi arguments on the stack
  for (auto &Phi : Target->phis()) {
    auto *IncomingVal = Phi.getIncomingValueForBlock(Source);
    uint32_t DestLocal = F.getOrCreateLocal(&Phi, Source->getDataLayout());
    // Do not move anything into the target local - just keep it as is
    if (llvm::isa<llvm::PoisonValue>(IncomingVal) ||
        llvm::isa<llvm::UndefValue>(IncomingVal)) {
      continue;
    }

    if (putValueOnStack(IncomingVal, F.Body, M,
                        Source->getDataLayout().getPointerSizeInBits() == 64)) {
    } else if (llvm::isa<llvm::Argument>(IncomingVal) ||
               llvm::isa<llvm::Instruction>(IncomingVal)) {

      uint32_t SourceLocal =
          F.getOrCreateLocal(IncomingVal, Source->getDataLayout());
      if (SourceLocal == DestLocal) {
        continue;
      }
      F.Body.Insts.emplace_back(Opcode::LocalGet,
                                std::make_unique<LocalArg>(SourceLocal));
    } else {
      WATEVER_UNIMPLEMENTED("Unsupported phi argument {}",
                            IncomingVal->getNameOrAsOperand());
    }
    Destinations.push_back(DestLocal);
  }

  // Pop them into their destinations
  while (!Destinations.empty()) {
    F.Body.Insts.emplace_back(
        Opcode::LocalSet,
        std::make_unique<LocalArg>(Destinations.pop_back_val()));
  }

  // Backward branch (continue) or forward branch (exit)
  if (Target == Fallthrough) {
    WATEVER_LOG_TRACE("Target {} is fallthrough for {}", getBlockName(Target),
                      getBlockName(Source));
  } else if (DT.dominates(Target, Source) || isMergeNode(Target)) {
#ifdef WATEVER_LOGGING
    if (DT.dominates(Target, Source)) {
      WATEVER_LOG_TRACE("backwards branch from {} to {}", getBlockName(Source),
                        getBlockName(Target));
    } else {
      WATEVER_LOG_TRACE("forwards branch from {} to {}", getBlockName(Source),
                        getBlockName(Target));
    }
#endif
    F.Body.Insts.emplace_back(Opcode::Br, index(Target, Ctx));
  } else {
    WATEVER_LOG_TRACE("no branch needed from {} to {}, fall through",
                      getBlockName(Source), getBlockName(Target));
    WorkStack.push_back(ProcessTree(Target, Ty, Fallthrough));
  }
}

void FunctionLowering::translateBB(llvm::BasicBlock *BB) const {
  BlockLowering BL{BB, M, F};
  return BL.lower();
}

void FunctionLowering::getMergeChildren(
    const llvm::BasicBlock *R,
    llvm::SmallVectorImpl<llvm::BasicBlock *> &Result) const {
  if (auto *Node = DT.getNode(R)) {
    for (const auto *Child : *Node) {
      if (isMergeNode(Child->getBlock()) ||
          llvm::isa<llvm::SwitchInst>(R->getTerminator())) {
        WATEVER_LOG_TRACE("{} is dominated merge",
                          getBlockName(Child->getBlock()));
        Result.push_back(Child->getBlock());
      }
    }
  }

  // Higher post-order-number comes first. This ensures that the first-popped
  // child can reach any other child and can safely be translated first
  std::ranges::sort(Result,
                    [&](const llvm::BasicBlock *A, const llvm::BasicBlock *B) {
                      return RPOOrdering.lookup(A) > RPOOrdering.lookup(B);
                    });
}

// TODO This is currently O(n^2)
void FunctionLowering::peephole() {
  WasmInst *Last = nullptr;
  for (auto *It = F.Body.Insts.begin(); It != F.Body.Insts.end();) {
    bool Erased = false;
    // local.set a; local.get a; -> local.tee a;
    if (It->Op == Opcode::LocalGet) {
      if (Last && Last->Op == Opcode::LocalSet) {
        const auto *Arg = llvm::cast<LocalArg>(It->getArgument());
        const auto *LastArg = llvm::cast<LocalArg>(Last->getArgument());
        if (Arg->Index == LastArg->Index) {
          Last->Op = Opcode::LocalTee;
          Erased = true;
          It = F.Body.Insts.erase(It);
        }
      }
    }

    // local.get a; local.set a; -> ;
    if (It->Op == Opcode::LocalSet) {
      if (Last && Last->Op == Opcode::LocalGet) {
        const auto *Arg = llvm::cast<LocalArg>(It->getArgument());
        const auto *LastArg = llvm::cast<LocalArg>(Last->getArgument());
        if (Arg->Index == LastArg->Index) {
          Erased = true;
          It = F.Body.Insts.erase(Last, std::next(It));
          Last = nullptr;
        }
      }
    }

    if (!Erased) {
      Last = It;
      ++It;
    }
  }
}

void FunctionLowering::removeUnusedLocals() {
  llvm::DenseSet<uint32_t> UsedLocals;
  for (const auto &Inst : F.Body.Insts) {
    if (const auto *Arg =
            llvm::dyn_cast_or_null<LocalArg>(Inst.getArgument())) {
      UsedLocals.insert(Arg->Index);
    }
  }

  for (auto &[_, LocalList] : F.Locals) {
    llvm::erase_if(LocalList, [&](auto L) { return !UsedLocals.contains(L); });
  }
}

void FunctionLowering::lower(ValType RetTy) {
  WorkStack.push_back(ProcessTree(DT.getRoot(), RetTy, nullptr));

  while (!WorkStack.empty()) {
    Task CurrentTask = WorkStack.pop_back_val();
    std::visit(Overloaded{[&](ProcessTree &T) {
                            processTree(T.Root, T.Ty, T.Fallthrough);
                          },
                          [&](HandleEdge &T) {
                            handleEdge(T.Source, T.Target, T.Ty, T.Fallthrough);
                          },
                          [&](EmitOp &T) { F.Body.Insts.emplace_back(T.Op); },
                          [&](PopContext &) { Ctx.Enclosing.pop_back(); }},
               CurrentTask);
  }

  peephole();
  removeUnusedLocals();
}

static void applyFeatures(DefinedFunc *Fn, llvm::StringRef FeatureString) {
  llvm::SmallVector<llvm::StringRef, 8> Features;
  FeatureString.split(Features, ',');

  for (llvm::StringRef Feature : Features) {
    Feature = Feature.trim();
    if (Feature.empty())
      continue;

    bool Enable = Feature.starts_with("+");
    llvm::StringRef Name = Feature.drop_front(1);

#define WATEVER_FEATURE(VAR, NAME, DEFAULT, HELP)                              \
  if (Name == #VAR)                                                            \
    Fn->FeatureSet.set_##VAR##_enabled(Enable);
#include "watever/feature.def"
#undef WATEVER_FEATURE
  }
}

Module ModuleLowering::convert(llvm::Module &Mod,
                               llvm::FunctionAnalysisManager &FAM,
                               TargetConfig C) {
  Module Res{};

  uint32_t FunctionIndexCounter = 0;
  llvm::DenseMap<RelocationEntry *, const llvm::GlobalValue *> FixUps;

  for (auto &GV : Mod.globals()) {
    WATEVER_LOG_TRACE("Adding global value {}", GV.getName().str());

    if (GV.isDeclaration()) {
      auto UndefData = std::make_unique<UndefinedData>(Res.Symbols.size(),
                                                       GV.getName().str());
      UndefData->setFlag(SymbolFlag::WASM_SYM_UNDEFINED);
      Res.DataMap[&GV] = UndefData.get();
      Res.Symbols.push_back(std::move(UndefData));
      continue;
    }

    std::vector<uint8_t> Content;
    llvm::SmallVector<std::unique_ptr<RelocationEntry>> Relocs;

    Res.flattenConstant(GV.getInitializer(), Content, Relocs, FixUps,
                        Mod.getDataLayout());

    DataSection S;
    if (GV.isConstant()) {
      S = DataSection::RO_DATA;
    } else if (GV.getInitializer()->isNullValue() ||
               GV.getInitializer()->isZeroValue()) {
      S = DataSection::BSS;
    } else {
      S = DataSection::DATA;
    }

    auto DefData = std::make_unique<DefinedData>(
        Res.Symbols.size(), Res.Datas.size(), true, GV.getName().str(), Content,
        std::move(Relocs), S);

    if (C.EnabledFeatures.atomics_enabled() &&
        GV.getThreadLocalMode() != llvm::GlobalValue::NotThreadLocal) {
      DefData->setSegmentFlag(SegmentFlag::WASM_SEGMENT_FLAG_TLS);
    }
    DefData->setFlags(GV);
    // TODO check if string or retain flags needed
    // TODO set alignment
    Res.DataMap[&GV] = DefData.get();
    Res.Datas.push_back(DefData.get());
    Res.Symbols.push_back(std::move(DefData));
  }

  // Imported functions must be defined first
  for (auto &F : Mod) {
    // Intrinsics will not get imported, nor called - but directly lowered
    // in place
    if (!F.isDeclaration() || F.isIntrinsic()) {
      continue;
    }
    auto *FT = F.getFunctionType();
    FuncType WasmFuncTy{};
    for (auto *ParamTy : FT->params()) {
      auto WasmType = fromLLVMType(ParamTy, Mod.getDataLayout());
      WasmFuncTy.Params.push_back(WasmType);
    }

    if (!FT->getReturnType()->isVoidTy()) {
      WasmFuncTy.Results.push_back(
          fromLLVMType(FT->getReturnType(), Mod.getDataLayout()));
    }

    const uint32_t FuncTypeIndex = Res.getOrAddType(WasmFuncTy);
    auto Import = std::make_unique<ImportedFunc>(
        Res.Symbols.size(), FuncTypeIndex, FunctionIndexCounter++,
        F.getName().str());
    Import->setFlag(SymbolFlag::WASM_SYM_UNDEFINED);
    Import->setFlags(F);
    Res.FunctionMap[&F] = Import.get();
    Res.Imports.push_back(Import.get());
    Res.Symbols.push_back(std::move(Import));
  }

  // Then declare all defined functions. This needs to be done before
  // lowering, so we can use not-yet-lowered functions (e.g., in calls).
  for (auto &F : Mod) {
    if (F.isDeclaration()) {
      continue;
    }

    auto *FT = F.getFunctionType();

    FuncType WasmFuncTy{};
    for (auto *ParamTy : FT->params()) {
      auto WasmType = fromLLVMType(ParamTy, Mod.getDataLayout());
      WasmFuncTy.Params.push_back(WasmType);
    }

    if (!FT->getReturnType()->isVoidTy()) {
      WasmFuncTy.Results.push_back(
          fromLLVMType(FT->getReturnType(), Mod.getDataLayout()));
    }

    const uint32_t FuncTypeIndex = Res.getOrAddType(WasmFuncTy);

    auto FunctionPtr = std::make_unique<DefinedFunc>(
        Res.Symbols.size(), FuncTypeIndex, FunctionIndexCounter++, &F,
        C.EnabledFeatures);

    FunctionPtr->setFlags(F);
    Res.FunctionMap[&F] = FunctionPtr.get();
    Res.Functions.push_back(FunctionPtr.get());
    Res.Symbols.push_back(std::move(FunctionPtr));
  }

  // Lower functions, which might involve adding a prologue to save the SP
  for (auto &F : Mod) {
    if (F.isDeclaration()) {
      continue;
    }
    auto *WasmFunc = static_cast<DefinedFunc *>(Res.FunctionMap[&F]);

    if (auto Attr = F.getFnAttribute("target-features"); Attr.isValid()) {
      applyFeatures(WasmFunc, Attr.getValueAsString());
    }
    // The module must enable all features used by any function
    Res.Config.EnabledFeatures.merge(WasmFunc->FeatureSet);

    auto &DT = FAM.getResult<llvm::DominatorTreeAnalysis>(F);
    auto &LI = FAM.getResult<llvm::LoopAnalysis>(F);

    if (C.DoColoring) {
      FunctionColorer FC{F, WasmFunc, DT, FAM};
      WATEVER_LOG_DBG("Coloring function {}", F.getName().str());
      FC.run();
    }

    WasmFunc->setupStackFrame(&F.front());
    // Generate prologue, if we use a FP
    if (WasmFunc->FP.has_value()) {
      const bool Is64Bit = Mod.getDataLayout().getPointerSizeInBits() == 64;
      const auto ConstOp = Is64Bit ? Opcode::I64Const : Opcode::I32Const;
      const auto SubOp = Is64Bit ? Opcode::I64Sub : Opcode::I32Sub;
      const auto PtrTy = Is64Bit ? ValType::I64 : ValType::I32;
      // SP = SP - frame_size
      WasmFunc->Body.Insts.emplace_back(Opcode::GlobalGet,
                                        Res.getStackPointer(PtrTy));
      WasmFunc->Body.Insts.emplace_back(ConstOp, WasmFunc->FrameSize);
      WasmFunc->Body.Insts.emplace_back(SubOp);
      WasmFunc->Body.Insts.emplace_back(
          Opcode::LocalTee, std::make_unique<LocalArg>(WasmFunc->FP.value()));
      WasmFunc->Body.Insts.emplace_back(Opcode::GlobalSet, Res.StackPointer);
    }

    FunctionLowering FL{*WasmFunc, DT, LI, Res};
    WATEVER_LOG_DBG("Lowering function {}", F.getName().str());
    ValType RetTy = ValType::Void;
    if (!F.getReturnType()->isVoidTy()) {
      RetTy = fromLLVMType(F.getReturnType(), Mod.getDataLayout());
    }

    FL.lower(RetTy);

#ifdef WATEVER_LOGGING
    dumpWasm(WasmFunc->Body);
#endif
  }

  for (auto &Alias : Mod.aliases()) {
    if (auto *F = llvm::dyn_cast<llvm::Function>(
            Alias.getAliasee()->stripPointerCasts())) {
      if (auto *AliaseeFunc = Res.FunctionMap.lookup(F)) {
        auto AliasPtr = std::make_unique<AliasedFunc>(
            Res.Symbols.size(), Alias.getName().str(), AliaseeFunc);

        if (Alias.hasHiddenVisibility()) {
          AliasPtr->setFlag(SymbolFlag::WASM_SYM_VISIBILITY_HIDDEN);
        }

        Res.Symbols.push_back(std::move(AliasPtr));
      } else {
        WATEVER_LOG_WARN("Trying to alias unknown function {}", F->getName());
      }
    }
  }

  for (auto &[Entry, GV] : FixUps) {
    if (auto *F = llvm::dyn_cast<llvm::Function>(GV)) {
      if (auto *WasumFunc = Res.FunctionMap[F]) {
        Entry->Index = WasumFunc->SymbolIndex;
      } else {
        WATEVER_UNREACHABLE("function not found as a symbol: {}", F->getName());
      }
    } else {
      if (auto *WasmData = Res.DataMap.lookup(GV)) {
        Entry->Index = WasmData->SymbolIndex;
      } else {
        WATEVER_UNREACHABLE("global variable not found as a symbol: {}",
                            GV->getNameOrAsOperand());
      }
    }
  }

  return Res;
}
