#pragma once

#include "watever/utils.hpp"
#include <cstdint>
#include <llvm/Support/LEB128.h>
#include <llvm/Support/raw_ostream.h>
#include <memory>
#include <string>

namespace watever {

enum class ExternalType : uint8_t {
  FuncType = 0,
  TableType = 1,
  MemType = 2,
  GlobalType = 3,
  TagType = 4,
};

class Limit {
  bool Is64Bit;
  bool HasMax;
  uint8_t getPrecedingFlag() const { return Is64Bit << 2 | HasMax; }

public:
  uint64_t Min;
  uint64_t Max;
  explicit Limit(uint32_t Min) : Is64Bit(false), HasMax(false), Min(Min) {}
  explicit Limit(uint64_t Min) : Is64Bit(true), HasMax(false), Min(Min) {}

  explicit Limit(uint32_t Min, uint32_t Max)
      : Is64Bit(false), HasMax(true), Min(Min), Max(Max) {}
  explicit Limit(uint64_t Min, uint64_t Max)
      : Is64Bit(true), HasMax(true), Min(Min), Max(Max) {}

  void writePayload(llvm::raw_ostream &OS) const {
    OS << getPrecedingFlag();
    llvm::encodeULEB128(Min, OS);
    if (HasMax) {
      llvm::encodeULEB128(Max, OS);
    }
  }
};

struct ExternType {
  virtual ~ExternType() = default;
  virtual void writePayload(llvm::raw_ostream &OS) const = 0;
  virtual ExternalType getExternalType() const = 0;
};

class MemType final : public ExternType {
  Limit Lim;

public:
  explicit MemType(Limit Lim) : Lim(Lim) {};

  void writePayload(llvm::raw_ostream &OS) const override {
    Lim.writePayload(OS);
  }

  ExternalType getExternalType() const override {
    return ExternalType::MemType;
  }
};

struct Import {
  std::string Name1;
  std::string Name2;

  std::unique_ptr<ExternType> Extern;
  Import(std::string Name1, std::string Name2,
         std::unique_ptr<ExternType> Extern)
      : Name1(std::move(Name1)), Name2(std::move(Name2)),
        Extern(std::move(Extern)) {}
};
} // namespace watever
