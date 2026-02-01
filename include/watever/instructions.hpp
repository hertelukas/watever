#pragma once

#include "watever/linking.hpp"
#include "watever/opcode.hpp"
#include "watever/type.hpp"
#include <llvm/Support/FormatVariadic.h>
#include <llvm/Support/LEB128.h>
#include <llvm/Support/raw_ostream.h>
#include <memory>

namespace watever {
struct Global;
struct ImportedGlobal;
struct Function;
struct Data;
struct Table;

class InstArgument {
public:
  enum class InstArgKind : uint8_t {
    Mem,
    Local,
    BranchTable,
    RelocatableFunc,
    RelocatableGlobal,
    RelocatablePointer,
    RelocatableIndirectCall,
    RelocatableTableIndex,
    MemCpy,
    BlockType,
  };

private:
  const InstArgKind Kind;

public:
  InstArgument(InstArgKind K) : Kind(K) {}
  [[nodiscard]] InstArgKind getKind() const { return Kind; }
  virtual void encode(llvm::raw_ostream &) const = 0;
  virtual void addRelocation(llvm::raw_ostream &, Relocation &) {};
  virtual void mapLocal(const llvm::SmallVector<uint32_t> &) {};
  [[nodiscard]] virtual std::string getString() const = 0;
  virtual ~InstArgument() = default;
};

class MemArg final : public InstArgument {
  uint32_t Alignment{};
  uint32_t MemIdx{};
  uint64_t Offset{};

public:
  MemArg() : InstArgument(InstArgKind::Mem) {}
  MemArg(uint32_t Alignment, uint64_t Offset)
      : InstArgument(InstArgKind::Mem), Alignment(Alignment), Offset(Offset) {}

  [[nodiscard]] std::string getString() const override {
    return llvm::formatv("align: {}, idx: {}, offset: {}", Alignment, MemIdx,
                         Offset);
  }

  void encode(llvm::raw_ostream &OS) const override {
    if (Alignment >= 64) {
      WATEVER_LOG_ERR("Alignment is too large: {}", Alignment);
    }
    uint32_t Flag = Alignment;

    if (MemIdx != 0) {
      Flag |= 64; // Signal that we have a MemIdx
    }
    llvm::encodeULEB128(Flag, OS);
    if (MemIdx != 0) {
      llvm::encodeULEB128(MemIdx, OS);
    }
    llvm::encodeULEB128(Offset, OS);
  }

  static bool classof(const InstArgument *IA) {
    return IA->getKind() == InstArgKind::Mem;
  }
};

class LocalArg final : public InstArgument {
public:
  uint32_t Index;
  explicit LocalArg(uint32_t L) : InstArgument(InstArgKind::Local), Index(L) {}

  [[nodiscard]] std::string getString() const override {
    return llvm::formatv("{}", Index);
  }
  void encode(llvm::raw_ostream &OS) const override {
    llvm::encodeULEB128(Index, OS);
  }

  void mapLocal(const llvm::SmallVector<uint32_t> &Mapping) override {
    if (Index < Mapping.size()) {
      Index = Mapping[Index];
    } else {
      WATEVER_LOG_WARN("Could not find mapping for local {}", Index);
    }
  }
  static bool classof(const InstArgument *IA) {
    return IA->getKind() == InstArgKind::Local;
  }
};

class BranchTableArg final : public InstArgument {
public:
  llvm::SmallVector<uint32_t> Targets;
  uint32_t DefaultTarget;

  explicit BranchTableArg(llvm::SmallVector<uint32_t> T, uint32_t D)
      : InstArgument(InstArgKind::BranchTable), Targets(std::move(T)),
        DefaultTarget(D) {}

  [[nodiscard]] std::string getString() const override {
    return llvm::formatv("[{0:$[, ]}] : {1}",
                         llvm::make_range(Targets.begin(), Targets.end()),
                         DefaultTarget);
  }

  void encode(llvm::raw_ostream &OS) const override {
    llvm::encodeULEB128(Targets.size(), OS);
    for (auto &T : Targets) {
      llvm::encodeULEB128(T, OS);
    }
    llvm::encodeULEB128(DefaultTarget, OS);
  }

  static bool classof(const InstArgument *IA) {
    return IA->getKind() == InstArgKind::BranchTable;
  }
};

class RelocatableFuncArg final : public InstArgument {
public:
  Function *Func;
  explicit RelocatableFuncArg(Function *F)
      : InstArgument(InstArgKind::RelocatableFunc), Func(F) {}

  [[nodiscard]] std::string getString() const override;
  void encode(llvm::raw_ostream &OS) const override;
  void addRelocation(llvm::raw_ostream &OS, Relocation &Reloc) override;

  static bool classof(const InstArgument *IA) {
    return IA->getKind() == InstArgKind::RelocatableFunc;
  }
};

class RelocatableGlobalArg final : public InstArgument {
  Global *Gl;

public:
  explicit RelocatableGlobalArg(Global *G)
      : InstArgument(InstArgKind::RelocatableGlobal), Gl(G) {}

  [[nodiscard]] std::string getString() const override;
  void encode(llvm::raw_ostream &OS) const override;
  void addRelocation(llvm::raw_ostream &OS, Relocation &Reloc) override;

  static bool classof(const InstArgument *IA) {
    return IA->getKind() == InstArgKind::RelocatableFunc;
  }
};

class RelocatablePointerArg final : public InstArgument {
  Data *DT;

public:
  explicit RelocatablePointerArg(Data *D)
      : InstArgument(InstArgKind::RelocatablePointer), DT(D) {}
  [[nodiscard]] std::string getString() const override;
  void encode(llvm::raw_ostream &OS) const override;
  void addRelocation(llvm::raw_ostream &OS, Relocation &Reloc) override;

  static bool classof(const InstArgument *IA) {
    return IA->getKind() == InstArgKind::RelocatablePointer;
  }
};

class RelocatableIndirectCallArg final : public InstArgument {
  uint32_t TypeIndex;
  Table *Tab;

public:
  explicit RelocatableIndirectCallArg(uint32_t TypeIdx,
                                      Table *IndirectFunctionTable)
      : InstArgument(InstArgKind::RelocatableIndirectCall), TypeIndex(TypeIdx),
        Tab(IndirectFunctionTable) {}

  [[nodiscard]] std::string getString() const override;
  void encode(llvm::raw_ostream &OS) const override;
  void addRelocation(llvm::raw_ostream &OS, Relocation &Reloc) override;

  static bool classof(const InstArgument *IA) {
    return IA->getKind() == InstArgKind::RelocatableIndirectCall;
  }
};

class RelocatableTableIndexArg final : public InstArgument {
  Function *F;

public:
  explicit RelocatableTableIndexArg(Function *F)
      : InstArgument(InstArgKind::RelocatableTableIndex), F(F) {}

  [[nodiscard]] std::string getString() const override;
  void encode(llvm::raw_ostream &OS) const override;
  void addRelocation(llvm::raw_ostream &OS, Relocation &Reloc) override;

  static bool classof(const InstArgument *IA) {
    return IA->getKind() == InstArgKind::RelocatableTableIndex;
  }
};

class MemCpyArg final : public InstArgument {
  uint32_t FromMemory;
  uint32_t ToMemory;

public:
  explicit MemCpyArg(uint32_t From, uint32_t To)
      : InstArgument(InstArgKind::MemCpy), FromMemory(From), ToMemory(To) {}

  [[nodiscard]] std::string getString() const override {
    return llvm::formatv("{} {}", FromMemory, ToMemory);
  }

  void encode(llvm::raw_ostream &OS) const override {
    llvm::encodeULEB128(FromMemory, OS);
    llvm::encodeULEB128(ToMemory, OS);
  }

  static bool classof(const InstArgument *IA) {
    return IA->getKind() == InstArgKind::MemCpy;
  }
};

class BlockTypeArg final : public InstArgument {
public:
  ValType Type;
  explicit BlockTypeArg(ValType Ty)
      : InstArgument(InstArgKind::BlockType), Type(Ty) {}

  [[nodiscard]] std::string getString() const override {
    return llvm::formatv("{}", static_cast<char>(Type));
  }

  void encode(llvm::raw_ostream &OS) const override {
    OS << static_cast<uint8_t>(Type);
  }

  static bool classof(const InstArgument *IA) {
    return IA->getKind() == InstArgKind::BlockType;
  }
};

class WasmInst {
  using Storage = std::variant<std::monostate, int64_t, uint64_t, float, double,
                               std::unique_ptr<InstArgument>>;
  Storage Arg;

public:
  [[nodiscard]] const InstArgument *getArgument() const {
    if (auto *Ptr = std::get_if<std::unique_ptr<InstArgument>>(&Arg)) {
      return Ptr->get();
    }
    return nullptr;
  }
  Opcode::Enum Op;

  WasmInst(Opcode::Enum O) : Arg(std::monostate{}), Op(O) {}
  WasmInst(Opcode::Enum O, int64_t Imm) : Arg(Imm), Op(O) {}
  WasmInst(Opcode::Enum O, uint64_t Imm) : Arg(Imm), Op(O) {}
  WasmInst(Opcode::Enum O, uint32_t Imm)
      : Arg(static_cast<uint64_t>(Imm)), Op(O) {}
  WasmInst(Opcode::Enum O, float Imm) : Arg(Imm), Op(O) {}
  WasmInst(Opcode::Enum O, double Imm) : Arg(Imm), Op(O) {}
  WasmInst(Opcode::Enum O, std::unique_ptr<InstArgument> Arg)
      : Arg(std::move(Arg)), Op(O) {}

  // Convenience constructors
  WasmInst(Opcode::Enum O, Global *IG)
      : Arg(std::make_unique<RelocatableGlobalArg>(IG)), Op(O) {}

  WasmInst(Opcode::Enum O, Data *D)
      : Arg(std::make_unique<RelocatablePointerArg>(D)), Op(O) {}

  WasmInst(Opcode::Enum O, Function *F)
      : Arg(std::make_unique<RelocatableFuncArg>(F)), Op(O) {}

  WasmInst(Opcode::Enum O, uint32_t From, uint32_t To)
      : Arg(std::make_unique<MemCpyArg>(From, To)), Op(O) {}

  WasmInst(WasmInst &&) = default;
  WasmInst &operator=(WasmInst &&) = default;

  static WasmInst createBlock(ValType Ty) {
    return {Opcode::Block, std::make_unique<BlockTypeArg>(Ty)};
  }

  static WasmInst createLoop(ValType Ty) {
    return {Opcode::Loop, std::make_unique<BlockTypeArg>(Ty)};
  }

  static WasmInst createIfElse(ValType Ty) {
    return {Opcode::If, std::make_unique<BlockTypeArg>(Ty)};
  }

  WasmInst(const WasmInst &) = delete;
  WasmInst &operator=(const WasmInst &) = delete;

#ifdef WATEVER_LOGGING
  [[nodiscard]] std::string getString() const {
    const char *Name = Opcode(Op).getName();

    return std::visit(
        Overloaded{
            [&](std::monostate) { return std::string(Name); },
            [&](int64_t Imm) {
              return llvm::formatv("{0} {1}", Name, Imm).str();
            },
            [&](uint64_t Imm) {
              return llvm::formatv("{0} {1}", Name, Imm).str();
            },
            [&](float Imm) {
              return llvm::formatv("{0} {1}", Name, Imm).str();
            },
            [&](double Imm) {
              return llvm::formatv("{0} {1}", Name, Imm).str();
            },

            [&](const std::unique_ptr<InstArgument> &Arg) {
              return llvm::formatv("{0} {1}", Name, Arg->getString()).str();
            },
        },
        Arg);
  }
#endif

  void write(llvm::raw_ostream &OS, Relocation &Reloc,
             llvm::SmallVector<uint32_t> &LocalMap) const {
    Opcode(Op).writeBytes(OS);

    std::visit(
        Overloaded{
            [&](std::monostate) {},
            [&](int64_t Imm) { llvm::encodeSLEB128(Imm, OS); },
            [&](uint64_t Imm) { llvm::encodeULEB128(Imm, OS); },
            [&](float Imm) {
              OS.write(reinterpret_cast<const char *>(&Imm), sizeof(Imm));
            },
            [&](double Imm) {
              OS.write(reinterpret_cast<const char *>(&Imm), sizeof(Imm));
            },
            [&](const std::unique_ptr<InstArgument> &Arg) {
              Arg->mapLocal(LocalMap);
              Arg->addRelocation(OS, Reloc);
              Arg->encode(OS);
            },
        },
        Arg);
  }
};

class WasmActions {
public:
  llvm::SmallVector<WasmInst, 8> Insts;
};

} // namespace watever
