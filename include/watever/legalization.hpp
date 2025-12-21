#pragma once

#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/Analysis.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/InstVisitor.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/Casting.h>

#include "watever/target.hpp"
#include "watever/utils.hpp"

namespace watever {

class LegalValue {
  // Keep i128 and less on the stack
  llvm::SmallVector<llvm::Value *, 2> Parts;

public:
  LegalValue() = default;
  LegalValue(llvm::Value *Single) { Parts.push_back(Single); }

  LegalValue(llvm::ArrayRef<llvm::Value *> Vs) : Parts(Vs.begin(), Vs.end()) {}

  using iterator = llvm::SmallVectorImpl<llvm::Value *>::const_iterator;

  [[nodiscard]] iterator begin() const { return Parts.begin(); }
  [[nodiscard]] iterator end() const { return Parts.end(); }
  [[nodiscard]] size_t size() const { return Parts.size(); }
  [[nodiscard]] bool empty() const { return Parts.empty(); }

  llvm::Value *operator[](size_t I) const {
    assert(I < Parts.size() && "Index out of bounds");
    return Parts[I];
  }

  [[nodiscard]] bool isScalar() const { return Parts.size() == 1; }
};

class LegalType {
  llvm::SmallVector<llvm::Type *, 2> Types;

public:
  LegalType(llvm::Type *T) { Types.push_back(T); }
  LegalType(llvm::ArrayRef<llvm::Type *> Ts) : Types(Ts.begin(), Ts.end()) {}

  using iterator = llvm::SmallVectorImpl<llvm::Type *>::const_iterator;

  [[nodiscard]] iterator begin() const { return Types.begin(); }
  [[nodiscard]] iterator end() const { return Types.end(); }
  [[nodiscard]] size_t size() const { return Types.size(); }
  [[nodiscard]] bool empty() const { return Types.empty(); }

  llvm::Type *operator[](size_t I) const {
    assert(I < Types.size() && "Index out of bounds");
    return Types[I];
  }

  [[nodiscard]] bool isScalar() const { return Types.size() == 1; }
};

class FunctionLegalizer : public llvm::InstVisitor<FunctionLegalizer> {

  // Maps Old Value -> New Legal Value
  llvm::DenseMap<llvm::Value *, LegalValue> ValueMap{};
  llvm::IRBuilder<> &Builder;
  const TargetConfig &Config;
  const llvm::DenseMap<llvm::Function *, llvm::Function *> &FuncMap;
  llvm::SmallVector<llvm::PHINode *> PHIsToFix;

  llvm::Type *Int1Ty;
  llvm::Type *Int8Ty;
  llvm::Type *Int16Ty;
  llvm::Type *Int32Ty;
  llvm::Type *Int64Ty;
  llvm::Type *PtrTy;
  llvm::Type *IntPtrTy;

  LegalValue legalizeConstant(llvm::Constant *C);

  LegalValue getMappedValue(llvm::Value *OldVal) {
    WATEVER_LOG_TRACE("getting mapped value for {}", llvmToString(*OldVal));
    // Value is mapped
    if (auto It = ValueMap.find(OldVal); It != ValueMap.end()) {
      return It->second;
    }

    // Value is not mapped, it might be a constant
    // TODO think about too-wide constants
    if (auto *C = llvm::dyn_cast<llvm::Constant>(OldVal)) {
      return legalizeConstant(C);
    }

    WATEVER_UNREACHABLE("No value found for {}", llvmToString(*OldVal));
  }

  // Zero-extends val from `From` to `To` bits. Note, that `Val` might already
  // be `To` bits wide, but this ensures that the entire `To` bits are zeroed
  // above `From`. This is needed if we cannot ignore the upper bits. (udiv)
  llvm::Value *zeroExtend(llvm::Value *Val, unsigned From, unsigned To) {
    // If the original value had the same width, we don't need to do anything
    if (To == From) {
      return Val;
    }

    // TODO check if it makes to use native extensions, LLVM doesn't seem to do
    // this

    if (To != 32 && To != 64) {
      WATEVER_UNIMPLEMENTED("Unsupported zero extension to {}", To);
    }

    llvm::APInt Mask = llvm::APInt::getLowBitsSet(To, From);
    return Builder.CreateAnd(Val, Mask);
  }

  // Sign-extends val from `From` to `To` bits. Note, that `Val` might already
  // be `To` bits wide, but this ensures that the entire `To` bits are signed
  // correctly. This is needed if we cannot ignore the upper bits, and sign
  // matters. (sdiv)
  llvm::Value *signExtend(llvm::Value *Val, unsigned From, unsigned To) {

    // If the original value had the same width, we don't need to do anything
    if (To == From) {
      return Val;
    }

    // Supported natively
    if (To == 64 && From == 32 && Val->getType()->getIntegerBitWidth() == 32) {
      return Builder.CreateSExt(Val, Int64Ty);
    }

    llvm::Type *TargetTy = nullptr;
    if (To == 32) {
      TargetTy = Int32Ty;
    } else if (To == 64) {
      TargetTy = Int64Ty;
    } else {
      WATEVER_UNIMPLEMENTED("Unsupported sign extension to {}", To);
    }

    // TODO check for --enable-sign-extension
    auto *WideOperand = Builder.CreateZExt(Val, TargetTy);
    auto *MaskVal = llvm::ConstantInt::get(TargetTy, To - From);
    llvm::Value *Shl = Builder.CreateShl(WideOperand, MaskVal);
    llvm::Value *Shr = Builder.CreateAShr(Shl, MaskVal);
    return Shr;
  }

public:
  llvm::Function *NewFunc;
  FunctionLegalizer(
      llvm::Function *OldFunc, llvm::Function *NewFunc, llvm::IRBuilder<> &B,
      const TargetConfig &Config,
      const llvm::DenseMap<llvm::Function *, llvm::Function *> &FuncMap);

  void fixupPHIs();

  void visitBasicBlock(llvm::BasicBlock &BB);

  // Terminator Instructions
  void visitReturnInst(llvm::ReturnInst &RI);
  void visitBranchInst(llvm::BranchInst &BI);

  // Unary Operations
  void visitUnaryOperator(llvm::UnaryOperator &UO);

  // Binary Operations
  void visitBinaryOperator(llvm::BinaryOperator &BO);

  // Vector Operations

  // Aggregatge Operations

  // Memory Access and Addressing Operations
  void visitAllocaInst(llvm::AllocaInst &AI);
  void visitLoadInst(llvm::LoadInst &LI);
  void visitStoreInst(llvm::StoreInst &SI);
  void visitGetElementPtrInst(llvm::GetElementPtrInst &GI);

  // Conversion Operations

  // Other Operations
  void visitICmpInst(llvm::ICmpInst &ICI);
  void visitFCmpInst(llvm::FCmpInst &FCI);
  void visitPHINode(llvm::PHINode &PN);
  void visitSelectInst(llvm::SelectInst &SI);
  void visitCallInst(llvm::CallInst &CI);

  void visitInstruction(llvm::Instruction &I) {
    // TODO set to UNIMPLEMENTED
    WATEVER_TODO("{} not (yet) supported", I.getOpcodeName());
  }
};

class LegalizationPass : public llvm::PassInfoMixin<LegalizationPass> {
  const TargetConfig Config;
  static llvm::Function *createLegalFunction(llvm::Module &M,
                                             llvm::Function *OldFunc);

  static llvm::FunctionType *
  createLegalFunctionType(llvm::FunctionType *OldFuncTy);

public:
  explicit LegalizationPass(const TargetConfig &C) : Config(C) {}
  static LegalType getLegalType(llvm::Type *Ty);
  llvm::PreservedAnalyses run(llvm::Module &M,
                              llvm::ModuleAnalysisManager &MAM);
};
} // namespace watever
