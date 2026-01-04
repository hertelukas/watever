#pragma once

#include "watever/linking.hpp"
#include "watever/opcode.hpp"
#include "watever/symbol.hpp"
#include <llvm/Support/FormatVariadic.h>
#include <llvm/Support/LEB128.h>
#include <llvm/Support/raw_ostream.h>

namespace watever {
class InstArgument {
public:
  virtual void encode(llvm::raw_ostream &) const = 0;
  virtual void addRelocation(llvm::raw_ostream &, Relocation &) {};
  [[nodiscard]] virtual std::string getString() const = 0;
  virtual ~InstArgument() = default;
};

class MemArg final : public InstArgument {
  uint32_t Alignment{};
  uint32_t MemIdx{};
  uint64_t Offset{};

public:
  MemArg() = default;
  MemArg(uint32_t Alignment, uint64_t Offset)
      : Alignment(Alignment), Offset(Offset) {}

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
};

class LocalArg final : public InstArgument {
public:
  Local *Lo;
  explicit LocalArg(Local *L) : Lo(L) {}
  [[nodiscard]] std::string getString() const override {
    return llvm::formatv("{}", Lo->Index);
  }
  void encode(llvm::raw_ostream &OS) const override {
    llvm::encodeULEB128(Lo->Index, OS);
  }
};

class BranchTableArg final : public InstArgument {
public:
  llvm::SmallVector<uint32_t> Targets;
  uint32_t DefaultTarget;

  explicit BranchTableArg(llvm::SmallVector<uint32_t> T, uint32_t D)
      : Targets(std::move(T)), DefaultTarget(D) {}

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
};

class RelocatableFuncArg final : public InstArgument {
public:
  Function *Func;
  explicit RelocatableFuncArg(Function *F) : Func(F) {}
  [[nodiscard]] std::string getString() const override {
    return llvm::formatv("{}", Func->FunctionIndex);
  }
  void encode(llvm::raw_ostream &OS) const override {
    // Must be padded to 5 bytes, so it can be patched by the linker
    llvm::encodeULEB128(Func->FunctionIndex, OS, 5);
  }

  void addRelocation(llvm::raw_ostream &OS, Relocation &Reloc) override {
    Reloc.Entries.emplace_back(RelocationType::R_WASM_FUNCTION_INDEX_LEB,
                               OS.tell(), Func->SymbolIndex);
  }
};

class RelocatableGlobalArg final : public InstArgument {
  Global *Gl;

public:
  explicit RelocatableGlobalArg(Global *G) : Gl(G) {}
  [[nodiscard]] std::string getString() const override {
    return llvm::formatv("{}", Gl->GlobalIdx);
  }
  void encode(llvm::raw_ostream &OS) const override {
    llvm::encodeULEB128(Gl->GlobalIdx, OS, 5);
  }

  void addRelocation(llvm::raw_ostream &OS, Relocation &Reloc) override {
    Reloc.Entries.emplace_back(RelocationType::R_WASM_GLOBAL_INDEX_LEB,
                               OS.tell(), Gl->SymbolIndex);
  }
};

class RelocatablePointerArg final : public InstArgument {
  Data *DT;

public:
  explicit RelocatablePointerArg(Data *D) : DT(D) {}
  [[nodiscard]] std::string getString() const override {
    return llvm::formatv("{}", DT->Name);
  }
  void encode(llvm::raw_ostream &OS) const override {
    llvm::encodeSLEB128(0, OS, 5);
  }

  void addRelocation(llvm::raw_ostream &OS, Relocation &Reloc) override {
    Reloc.Entries.emplace_back(RelocationType::R_WASM_MEMORY_ADDR_SLEB,
                               OS.tell(), DT->SymbolIndex);
  }
};

class RelocatableIndirectCallArg final : public InstArgument {
  uint32_t TypeIndex;
  Table *Tab;

public:
  explicit RelocatableIndirectCallArg(uint32_t TypeIdx,
                                      Table *IndirectFunctionTable)
      : TypeIndex(TypeIdx), Tab(IndirectFunctionTable) {}

  [[nodiscard]] std::string getString() const override {
    return llvm::formatv("{} {}", TypeIndex, Tab->TableIndex).str();
  }
  void encode(llvm::raw_ostream &OS) const override {
    llvm::encodeULEB128(TypeIndex, OS, 5);
    llvm::encodeULEB128(Tab->TableIndex, OS, 5);
  }

  void addRelocation(llvm::raw_ostream &OS, Relocation &Reloc) override {
    Reloc.Entries.emplace_back(RelocationType::R_WASM_TYPE_INDEX_LEB, OS.tell(),
                               TypeIndex);
    Reloc.Entries.emplace_back(RelocationType::R_WASM_TABLE_NUMBER_LEB,
                               OS.tell() + 5, Tab->SymbolIndex);
  }
};

class RelocatableTableIndexArg final : public InstArgument {
  Function *F;

public:
  explicit RelocatableTableIndexArg(Function *F) : F(F) {}

  [[nodiscard]] std::string getString() const override {
    return llvm::formatv("table index for {}", F->Name).str();
  }
  void encode(llvm::raw_ostream &OS) const override {
    llvm::encodeSLEB128(0, OS, 5);
  }

  void addRelocation(llvm::raw_ostream &OS, Relocation &Reloc) override {
    Reloc.Entries.emplace_back(RelocationType::R_WASM_TABLE_INDEX_SLEB,
                               OS.tell(), F->SymbolIndex);
  }
};

class MemCpyArg final : public InstArgument {
  uint32_t FromMemory;
  uint32_t ToMemory;

public:
  explicit MemCpyArg(uint32_t From, uint32_t To)
      : FromMemory(From), ToMemory(To) {}

  [[nodiscard]] std::string getString() const override {
    return llvm::formatv("{} {}", FromMemory, ToMemory);
  }

  void encode(llvm::raw_ostream &OS) const override {
    llvm::encodeULEB128(FromMemory, OS);
    llvm::encodeULEB128(ToMemory, OS);
  }
};

class WasmInst {
  using Storage =
      std::variant<std::monostate, int64_t, std::unique_ptr<InstArgument>>;
  Storage Arg;

public:
  Opcode::Enum Op;

  WasmInst(Opcode::Enum O) : Arg(std::monostate{}), Op(O) {}
  WasmInst(Opcode::Enum O, int64_t Imm) : Arg(Imm), Op(O) {}
  WasmInst(Opcode::Enum O, std::unique_ptr<InstArgument> Arg)
      : Arg(std::move(Arg)), Op(O) {}

  // Convenience constructors
  WasmInst(Opcode::Enum O, ImportedGlobal *IG)
      : Arg(std::make_unique<RelocatableGlobalArg>(IG)), Op(O) {}

  WasmInst(Opcode::Enum O, Local *L)
      : Arg(std::make_unique<LocalArg>(L)), Op(O) {}

  WasmInst(Opcode::Enum O, Data *D)
      : Arg(std::make_unique<RelocatablePointerArg>(D)), Op(O) {}

  WasmInst(Opcode::Enum O, Function *F)
      : Arg(std::make_unique<RelocatableFuncArg>(F)), Op(O) {}

  WasmInst(Opcode::Enum O, uint32_t From, uint32_t To)
      : Arg(std::make_unique<MemCpyArg>(From, To)), Op(O) {}

  WasmInst(WasmInst &&) = default;
  WasmInst &operator=(WasmInst &&) = default;

  WasmInst(const WasmInst &) = delete;
  WasmInst &operator=(const WasmInst &) = delete;

#ifdef WATEVER_LOGGING
  [[nodiscard]] std::string getString() const {
    const char *Name = Opcode(Op).getName();

    return std::visit(
        Overloaded{
            [&](std::monostate) { return std::string(Name); },
            [&](uint64_t Imm) {
              return llvm::formatv("{0} {1}", Name, Imm).str();
            },
            [&](const std::unique_ptr<InstArgument> &Arg) {
              return llvm::formatv("{0} {1}", Name, Arg->getString()).str();
            },
        },
        Arg);
  }
#endif

  void write(llvm::raw_ostream &OS, Relocation &Reloc) const {
    Opcode(Op).writeBytes(OS);

    std::visit(Overloaded{
                   [&](std::monostate) {},
                   [&](int64_t Imm) { llvm::encodeSLEB128(Imm, OS); },
                   [&](const std::unique_ptr<InstArgument> &Arg) {
                     Arg->addRelocation(OS, Reloc);
                     Arg->encode(OS);
                   },
               },
               Arg);
  }
};
} // namespace watever
