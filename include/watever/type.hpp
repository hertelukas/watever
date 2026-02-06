#pragma once

#include "watever/utils.hpp"
#include <algorithm>
#include <cstdint>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/LEB128.h>
#include <llvm/Support/raw_ostream.h>
#include <string_view>

namespace watever {
enum class ValType : uint8_t {
  // NumTypes
  I32 = 0x7F,
  I64 = 0x7E,
  F32 = 0x7D,
  F64 = 0x7C,

  // VecTypes
  V128 = 0x7B,

  // Abstract Heap Types
  Exn = 0x69,
  Array = 0x6A,
  Struct = 0x6B,
  I31 = 0x6C,
  Eq = 0x6D,
  Any = 0x6E,
  Extern = 0x6F,
  Func = 0x70,
  None = 0x71,
  NoExtern = 0x72,
  NoFunc = 0x73,
  NoExn = 0x74,

  // Internal
  Void = 0x40,
  ___ = Void,
};

constexpr bool isRef(ValType T) {
  return T >= ValType::Exn && T <= ValType::NoExn;
};

constexpr std::string_view toString(ValType T) {
  switch (T) {
  case ValType::I32:
    return "i32";
  case ValType::I64:
    return "i64";
  case ValType::F32:
    return "f32";
  case ValType::F64:
    return "f64";
  case ValType::V128:
    return "v128";
  case ValType::Exn:
    return "exnref";
  case ValType::Array:
    return "arrayref";
  case ValType::Struct:
    return "structref";
  case ValType::I31:
    return "i31ref";
  case ValType::Eq:
    return "eqref";
  case ValType::Any:
    return "anyref";
  case ValType::Extern:
    return "externref";
  case ValType::Func:
    return "funcref";
  case ValType::None:
    return "nullref";
  case ValType::NoExtern:
    return "noextern";
  case ValType::NoFunc:
    return "nofunc";
  case ValType::NoExn:
    return "noexn";
  case ValType::Void:
    return "";
  }
  return "unknown";
}

inline ValType fromLLVMType(llvm::Type *T, const llvm::DataLayout &DL) {
  switch (T->getTypeID()) {
  case llvm::Type::IntegerTyID: {
    const unsigned Width = T->getIntegerBitWidth();
    if (Width <= 32) {
      return ValType::I32;
    }
    if (Width == 64) {
      return ValType::I64;
    }
    break;
  }
  case llvm::Type::FloatTyID:
    return ValType::F32;
  case llvm::Type::DoubleTyID:
    return ValType::F64;
  case llvm::Type::PointerTyID: {
    if (DL.getPointerTypeSizeInBits(T) == 64) {
      return ValType::I64;
    }
    return ValType::I32;
  }
  default:
    break;
  }
  WATEVER_UNREACHABLE("unsupported type {}", llvmToString(*T));
}

using ResultType = llvm::SmallVector<ValType>;

struct FuncType {
  ResultType Params;
  ResultType Results;

  auto operator<=>(const FuncType &Other) const {
    if (auto Cmp = std::lexicographical_compare_three_way(
            Params.begin(), Params.end(), Other.Params.begin(),
            Other.Params.end());
        Cmp != 0) {
      return Cmp;
    }
    return std::lexicographical_compare_three_way(
        Results.begin(), Results.end(), Other.Results.begin(),
        Other.Results.end());
  }

  void encode(llvm::raw_ostream &OS) const {
    OS << uint8_t(0x60);
    llvm::encodeULEB128(Params.size(), OS);
    for (const auto &Param : Params) {
      OS << static_cast<uint8_t>(Param);
    }
    llvm::encodeULEB128(Results.size(), OS);
    for (const auto &Res : Results) {
      OS << static_cast<uint8_t>(Res);
    }
  }
};
} // namespace watever
