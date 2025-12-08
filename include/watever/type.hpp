/*
 * Copied from
 * https://github.com/WebAssembly/wabt/blob/main/include/wabt/type.h
 */
#pragma once

#include "watever/base-types.hpp"
#include "watever/utils.hpp"

#include <cassert>
#include <format>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/Type.h>
#include <string>
#include <vector>

namespace watever {

class Type;

using TypeVector = std::vector<Type>;

class Type {
public:
  // Matches binary format, do not change.
  enum Enum : uint8_t {
    I32 = 0x7F,
    I64 = 0x7E,
    F32 = 0x7D,
    F64 = 0x7C,
    V128 = 0x7B,
    I8 = 0x7A,
    I16 = 0x79,
    ExnRef = 0x69,
    FuncRef = 0x70,
    ExternRef = 0x6F,
    Reference = 0x6B,
    Func = 0x60,
    Struct = 0x5F,
    Array = 0x5E,
    Void = 0x40,
    ___ = Void, // Convenient for the opcode table in opcode.h

    Any = 0,  // Not actually specified, but useful for type-checking
    I8U = 4,  // Not actually specified, but used internally with load/store
    I16U = 6, // Not actually specified, but used internally with load/store
    I32U = 7, // Not actually specified, but used internally with load/store
  };

  Type() = default; // Provided so Type can be member of a union.
  Type(int32_t code) : enum_(static_cast<Enum>(code)), type_index_(0) {
    assert(!EnumIsReferenceWithIndex(enum_));
  }
  Type(Enum e) : enum_(e), type_index_(0) {
    assert(!EnumIsReferenceWithIndex(enum_));
  }
  Type(Enum e, Index type_index) : enum_(e), type_index_(type_index) {
    assert(EnumIsReferenceWithIndex(e));
  }
  constexpr operator Enum() const { return enum_; }

  friend constexpr bool operator==(const Type a, const Type b) {
    return a.enum_ == b.enum_ && a.type_index_ == b.type_index_;
  }
  friend constexpr bool operator!=(const Type a, const Type b) {
    return !(a == b);
  }
  friend constexpr bool operator==(const Type ty, const Enum code) {
    return ty.enum_ == code;
  }
  friend constexpr bool operator!=(const Type ty, const Enum code) {
    return !(ty == code);
  }
  friend constexpr bool operator<(const Type a, const Type b) {
    return a.enum_ == b.enum_ ? a.type_index_ < b.type_index_
                              : a.enum_ < b.enum_;
  }

  bool IsRef() const {
    return enum_ == Type::ExternRef || enum_ == Type::FuncRef ||
           enum_ == Type::Reference || enum_ == Type::ExnRef;
  }

  bool IsReferenceWithIndex() const { return EnumIsReferenceWithIndex(enum_); }

  bool IsNullableRef() const {
    // Currently all reftypes are nullable
    return IsRef();
  }

  std::string GetName() const {
    switch (enum_) {
    case Type::I32:
      return "i32";
    case Type::I64:
      return "i64";
    case Type::F32:
      return "f32";
    case Type::F64:
      return "f64";
    case Type::V128:
      return "v128";
    case Type::I8:
      return "i8";
    case Type::I16:
      return "i16";
    case Type::ExnRef:
      return "exnref";
    case Type::FuncRef:
      return "funcref";
    case Type::Func:
      return "func";
    case Type::Void:
      return "void";
    case Type::Any:
      return "any";
    case Type::ExternRef:
      return "externref";
    case Type::Reference:
      return std::format("(ref {})", type_index_);
    default:
      return std::format("<type_index[{}]>", static_cast<uint32_t>(enum_));
    }
  }

  const char *GetRefKindName() const {
    switch (enum_) {
    case Type::FuncRef:
      return "func";
    case Type::ExternRef:
      return "extern";
    case Type::ExnRef:
      return "exn";
    case Type::Struct:
      return "struct";
    case Type::Array:
      return "array";
    default:
      return "<invalid>";
    }
  }

  // Functions for handling types that are an index into the type section.
  // These are always positive integers. They occur in the binary format in
  // block signatures, e.g.
  //
  //   (block (result i32 i64) ...)
  //
  // is encoded as
  //
  //   (type $T (func (result i32 i64)))
  //   ...
  //   (block (type $T) ...)
  //
  // TODO very likely wrong
  bool IsIndex() const { return static_cast<int8_t>(enum_) >= 0; }

  Index GetIndex() const {
    assert(IsIndex());
    return static_cast<Index>(enum_);
  }

  Index GetReferenceIndex() const {
    assert(enum_ == Enum::Reference);
    return type_index_;
  }

  TypeVector GetInlineVector() const {
    assert(!IsIndex());
    switch (enum_) {
    case Type::Void:
      return TypeVector();

    case Type::I32:
    case Type::I64:
    case Type::F32:
    case Type::F64:
    case Type::V128:
    case Type::FuncRef:
    case Type::ExnRef:
    case Type::ExternRef:
    case Type::Reference:
      return TypeVector(this, this + 1);

    default:
      assert(false && "unreachable");
    }
  }

  static Type::Enum fromLLVMType(llvm::Type *T, const llvm::DataLayout &DL) {
    switch (T->getTypeID()) {
    case llvm::Type::IntegerTyID: {
      const unsigned Width = T->getIntegerBitWidth();
      if (Width == 32) {
        return Type::Enum::I32;
      }
      if (Width == 64) {
        return Type::Enum::I64;
      }
      break;
    }
    case llvm::Type::FloatTyID:
      return Type::Enum::F32;
    case llvm::Type::DoubleTyID:
      return Type::Enum::F64;
    case llvm::Type::PointerTyID: {
      if (DL.getPointerTypeSizeInBits(T) == 64) {
        return Type::Enum::I64;
      }
      return Type::Enum::I32;
    }
    default:
      break;
    }
    WATEVER_UNREACHABLE("unsupported type {}", llvmToString(*T));
  }

private:
  static bool EnumIsReferenceWithIndex(Enum value) {
    return value == Type::Reference;
  }

  Enum enum_;
  // This index is 0 for non-references, so a zeroed
  // memory area represents a valid Type::Any type.
  // It contains an index for references with type index.
  Index type_index_;
};
} // namespace watever
