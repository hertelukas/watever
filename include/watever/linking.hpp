#pragma once

#include "watever/utils.hpp"
#include <cstdint>
#include <llvm/Support/LEB128.h>
#include <llvm/Support/raw_ostream.h>
#include <variant>
#include <vector>
namespace watever {
static constexpr uint32_t LinkingVersion = 0x2;

enum class RelocationType : uint8_t {
  R_WASM_FUNCTION_INDEX_LEB = 0,
  R_WASM_TABLE_INDEX_SLEB = 1,
  R_WASM_TABLE_INDEX_I32 = 2,
  R_WASM_MEMORY_ADDR_LEB = 3,
  R_WASM_MEMORY_ADDR_SLEB = 4,
  R_WASM_MEMORY_ADDR_I32 = 5,
  R_WASM_TYPE_INDEX_LEB = 6,
  R_WASM_GLOBAL_INDEX_LEB = 7,
  R_WASM_FUNCTION_OFFSET_I32 = 8,
  R_WASM_SECTION_OFFSET_I32 = 9,
  R_WASM_EVENT_INDEX_LEB = 10,
  R_WASM_GLOBAL_INDEX_I32 = 13,
  R_WASM_MEMORY_ADDR_LEB64 = 14,
  R_WASM_MEMORY_ADDR_SLEB64 = 15,
  R_WASM_MEMORY_ADDR_I64 = 16,
  R_WASM_TABLE_INDEX_SLEB64 = 18,
  R_WASM_TABLE_INDEX_I64 = 19,
  R_WASM_TABLE_NUMBER_LEB = 20,
  R_WASM_FUNCTION_OFFSET_I64 = 22,
  R_WASM_MEMORY_ADDR_LOCREL_I32 = 23,
  R_WASM_TABLE_INDEX_REL_SLEB64 = 24,
  R_WASM_FUNCTION_INDEX_I32 = 26,
};

struct RelocationEntry {
  RelocationType Type;
  // offset of the value to rewrite (relative to the relevant section's
  // contents: offset zero is immediately after the id and size of the section)
  uint32_t Offset;
  // the index of the symbol used (or, for R_WASM_TYPE_INDEX_LEB relocations,
  // the index of the type)
  uint32_t Index;

  // For R_WASM_MEMORY_ADDR_*, R_WASM_FUNCTION_OFFSET_I32, and
  // R_WASM_SECTION_OFFSET_I32 relocations (and their 64-bit counterparts), an
  // addend which gets added to the address
  int32_t Addend{};

  RelocationEntry(RelocationType Ty, uint32_t Offset, uint32_t Idx)
      : Type(Ty), Offset(Offset), Index(Idx) {}

  [[nodiscard]] constexpr bool hasAddend() const {
    return (Type == RelocationType::R_WASM_MEMORY_ADDR_LEB ||
            Type == RelocationType::R_WASM_MEMORY_ADDR_SLEB ||
            Type == RelocationType::R_WASM_MEMORY_ADDR_I32 ||
            Type == RelocationType::R_WASM_MEMORY_ADDR_LEB64 ||
            Type == RelocationType::R_WASM_MEMORY_ADDR_SLEB64 ||
            Type == RelocationType::R_WASM_MEMORY_ADDR_I64 ||
            Type == RelocationType::R_WASM_MEMORY_ADDR_LOCREL_I32 ||
            Type == RelocationType::R_WASM_FUNCTION_OFFSET_I32 ||
            Type == RelocationType::R_WASM_FUNCTION_OFFSET_I64 ||
            Type == RelocationType::R_WASM_SECTION_OFFSET_I32);
  }
};

/// https://github.com/WebAssembly/tool-conventions/blob/main/Linking.md#relocation-sections
struct Relocation {
  // Index of target section (varuint32)
  uint32_t Section;
  std::vector<RelocationEntry> Entries;

  // Used for reloc.<SectionName>
  std::string SectionName;

  explicit Relocation(uint32_t Idx, std::string Name)
      : Section(Idx), SectionName(std::move(Name)) {}
};

enum class SectionType : uint8_t {
  WASM_SEGMENT_INFO = 5,
  WASM_INIT_FUNCS = 6,
  WASM_COMDAT_INFO = 7,
  WASM_SYMBOL_TABLE = 8,
};

struct Subsection {
  virtual ~Subsection() = default;
  // code identifying type of subsection
  [[nodiscard]] virtual SectionType getSectionType() const = 0;
  [[nodiscard]] virtual uint32_t getPayloadLen() const = 0;
  virtual void writePayload(llvm::raw_ostream &OS) const = 0;
};

enum class SegmentFlag : uint32_t { // NOLINT(performance-enum-size)
  // Signals that the segment contains only null terminated strings allowing the
  // linker to perform merging.
  WASM_SEGMENT_FLAG_STRINGS = 1,
  // The segment contains thread-local data. This means that a unique copy of
  // this segment will be created for each thread.
  WASM_SEGMENT_FLAG_TLS = 2,
  // If the object file is included in the final link, the segment should be
  // retained in the final output regardless of whether it is used by the
  // program.
  WASM_SEG_FLAG_RETAIN = 4,
};

struct Segment {
  // UTF-8 encoding of the segment's name
  std::string Name;
  // UTF-8 encoding of the segment's name (varuint32)
  uint32_t Alignment;
  // a bitfield containing flags for this segment (varuint32)
  uint32_t Flags;

  void setFlag(SegmentFlag Flag) { Flags |= static_cast<uint32_t>(Flag); }

  [[nodiscard]] uint32_t getSegmentLen() const {
    uint32_t Len = 0;
    Len += llvm::getULEB128Size(Name.size()); // name_len
    Len += Name.size();                       // name_data
    Len += llvm::getULEB128Size(Alignment);
    Len += llvm::getULEB128Size(Flags);
    return Len;
  }
};

struct SegmentInfo final : Subsection {
  std::vector<Segment> Segments;

  [[nodiscard]] SectionType getSectionType() const override {
    return SectionType::WASM_SEGMENT_INFO;
  }

  [[nodiscard]] uint32_t getPayloadLen() const override {
    uint32_t Res = llvm::getULEB128Size(Segments.size());
    for (auto &Seg : Segments) {
      Res += Seg.getSegmentLen();
    }
    return Res;
  }

  void writePayload(llvm::raw_ostream &OS) const override {
    llvm::encodeULEB128(Segments.size(), OS); // count
    for (auto &Seg : Segments) {
      llvm::encodeULEB128(Seg.Name.size(), OS);
      OS << Seg.Name;
      llvm::encodeULEB128(Seg.Alignment, OS);
      llvm::encodeULEB128(Seg.Flags, OS);
    }
  }
};

struct InitFunc {
  // priority of the init function (varuint32)
  uint32_t Priority;
  // the symbol index of init function (not the function index) (varuint32)
  uint32_t SymbolIndex;

  [[nodiscard]] uint32_t getInitFuncLen() const {
    uint32_t Len = llvm::getULEB128Size(Priority);
    Len += llvm::getULEB128Size(SymbolIndex);
    return Len;
  }
};

struct InitFunctions final : Subsection {
  std::vector<InitFunc> Functions;

  [[nodiscard]] SectionType getSectionType() const override {
    return SectionType::WASM_INIT_FUNCS;
  }

  [[nodiscard]] uint32_t getPayloadLen() const override {
    uint32_t Res = llvm::getULEB128Size(Functions.size());
    for (auto &F : Functions) {
      Res += F.getInitFuncLen();
    }
    return Res;
  }

  void writePayload(llvm::raw_ostream &OS) const override {
    llvm::encodeULEB128(Functions.size(), OS);
    for (auto &F : Functions) {
      llvm::encodeULEB128(F.Priority, OS);
      llvm::encodeULEB128(F.SymbolIndex, OS);
    }
  }
};

enum class SymbolKind : uint8_t {
  SYMTAB_FUNCTION = 0,
  SYMTAB_DATA = 1,
  SYMTAB_GLOBAL = 2,
  SYMTAB_SECTION = 3,
  SYMTAB_EVENT = 4,
  SYMTAB_TABLE = 5,
};

enum class SymbolFlag : uint32_t { // NOLINT(performance-enum-size)
  // Indicating that this is a weak symbol. When linking multiple modules
  // defining the same symbol, all weak definitions are discarded if any strong
  // definitions exist; then if multiple weak definitions exist all but one
  // (unspecified) are discarded; and finally it is an error if more than one
  // definition remains.
  WASM_SYM_BINDING_WEAK = 1,
  // Indicating that this is a local symbol (this is exclusive with
  // WASM_SYM_BINDING_WEAK). Local symbols are not to be exported, or linked to
  // other modules/sections. The names of all non-local symbols must be unique,
  // but the names of local symbols are not considered for uniqueness. A local
  // function or global symbol cannot reference an import.
  WASM_SYM_BINDING_LOCAL = 2,
  // Indicating that this is a hidden symbol. Hidden symbols are not to be
  // exported when performing the final link, but may be linked to other
  // modules.
  WASM_SYM_VISIBILITY_HIDDEN = 4,
  // Indicating that this symbol is not defined. For non-data symbols, this must
  // match whether the symbol is an import or is defined; for data symbols,
  // determines whether a segment is specified.
  WASM_SYM_UNDEFINED = 0x10,
  // The symbol is intended to be exported from the wasm module to the host
  // environment. This differs from the visibility flags in that it effects the
  // static linker.
  WASM_SYM_EXPORTED = 0x20,
  // The symbol is intended to be exported from the wasm module to the host
  // environment. This differs from the visibility flags in that it effects the
  // static linker.
  WASM_SYM_EXPLICIT_NAME = 0x40,
  // The symbol is intended to be included in the linker output, regardless of
  // whether it is used by the program.
  WASM_SYM_NO_STRIP = 0x80,
  // The symbol resides in thread local storage.
  WASM_SYM_TLS = 0x100,
  // The symbol represents an absolute address. This means it's offset is
  // relative to the start of the wasm memory as opposed to being relative to a
  // data segment.
  WASM_SYM_ABSOLUTE = 0x200,
};

// For FUNCTION, GLOBAL, EVENT, TABLE (afaik)
struct SymbolName {
  uint32_t Index;
  std::string Name;
  bool IsImport;

  [[nodiscard]] uint32_t getLen() const {
    uint32_t Len{};
    Len += llvm::getULEB128Size(Index);
    if (!IsImport) {
      Len += llvm::getULEB128Size(Name.size());
      Len += Name.size();
    }
    return Len;
  }
};

// For DATA
struct SymbolData {
  std::string Name;
  // TODO these are all optionals, see spec
  uint32_t Index;
  uint32_t Offset;
  uint32_t Size;

  [[nodiscard]] uint32_t getLen() const {
    uint32_t Len{};
    Len += llvm::getULEB128Size(Name.size());
    Len += Name.size();
    Len += llvm::getULEB128Size(Index);
    Len += llvm::getULEB128Size(Offset);
    Len += llvm::getULEB128Size(Size);
    return Len;
  }
};

// For SECTION
struct SymbolSection {
  uint32_t Section;

  [[nodiscard]] uint32_t getLen() const {
    return llvm::getULEB128Size(Section);
  }
};

struct SymInfo {
  SymbolKind Kind;
  uint32_t Flags{};

  std::variant<SymbolName, SymbolData, SymbolSection> Content;

  [[nodiscard]] uint32_t getSymInfoLen() const {
    uint32_t Len = 1; // kind
    Len += llvm::getULEB128Size(Flags);
    Len += std::visit([](const auto &Arg) { return Arg.getLen(); }, Content);
    return Len;
  }
};

struct SymbolTable final : Subsection {
  std::vector<SymInfo> Infos;

  [[nodiscard]] SectionType getSectionType() const override {
    return SectionType::WASM_SYMBOL_TABLE;
  }

  [[nodiscard]] uint32_t getPayloadLen() const override {
    uint32_t Res = llvm::getULEB128Size(Infos.size());
    for (auto &S : Infos) {
      Res += S.getSymInfoLen();
    }
    return Res;
  }

  void writePayload(llvm::raw_ostream &OS) const override {
    llvm::encodeULEB128(Infos.size(), OS);
    for (auto &S : Infos) {
      OS << static_cast<uint8_t>(S.Kind);
      llvm::encodeULEB128(S.Flags, OS);
      std::visit(Overloaded{[&](const SymbolName &SymName) {
                              llvm::encodeULEB128(SymName.Index, OS);
                              if (!SymName.IsImport) {
                                llvm::encodeULEB128(SymName.Name.size(), OS);
                                OS << SymName.Name;
                              }
                            },
                            [&](const SymbolData &SymData) {
                              llvm::encodeULEB128(SymData.Name.size(), OS);
                              OS << SymData.Name;
                              llvm::encodeULEB128(SymData.Index, OS);
                              llvm::encodeULEB128(SymData.Offset, OS);
                              llvm::encodeULEB128(SymData.Size, OS);
                            },
                            [&](const SymbolSection &SymSec) {
                              llvm::encodeULEB128(SymSec.Section, OS);
                            }},
                 S.Content);
    }
  }
};

enum class ComdatKind : uint8_t {
  WASM_COMDAT_DATA = 0,
  WASM_COMDAT_FUNCTION = 1,
  WASM_COMDAT_GLOBAL = 2,
  WASM_COMDAT_EVENT = 3,
  WASM_COMDAT_TABLE = 4,
  WASM_COMDAT_SECTION = 5,
};

struct ComdatSym {
  ComdatKind Kind;
  uint32_t Index;

  [[nodiscard]] uint32_t getComdatSymLen() const {
    return 1 + llvm::getULEB128Size(Index);
  }
};

struct Comdat {
  std::string Name;
  const uint32_t Flags = 0; // Must be zero
  std::vector<ComdatSym> ComdatSyms;
  [[nodiscard]] uint32_t getComdatLen() const {
    uint32_t Res = llvm::getULEB128Size(Name.size()); // name_len
    Res += Name.size();                               // name_str
    Res += llvm::getULEB128Size(Flags);               // flags
    Res += llvm::getULEB128Size(ComdatSyms.size());   // count
    for (auto &CS : ComdatSyms) {
      Res += CS.getComdatSymLen();
    }
    return Res;
  }
};

struct ComdatInfo final : Subsection {
  std::vector<Comdat> Comdats;

  [[nodiscard]] SectionType getSectionType() const override {
    return SectionType::WASM_COMDAT_INFO;
  }

  [[nodiscard]] uint32_t getPayloadLen() const override {
    uint32_t Res = llvm::getULEB128Size(Comdats.size());
    for (auto &C : Comdats) {
      Res += C.getComdatLen();
    }
    return Res;
  }

  void writePayload(llvm::raw_ostream &OS) const override {
    llvm::encodeULEB128(Comdats.size(), OS);
    for (auto &C : Comdats) {
      llvm::encodeULEB128(C.Name.size(), OS);
      OS << C.Name;
      llvm::encodeULEB128(C.Flags, OS);
      llvm::encodeULEB128(C.ComdatSyms.size(), OS);
      for (auto &CS : C.ComdatSyms) {
        OS << static_cast<uint8_t>(CS.Kind);
        llvm::encodeULEB128(CS.Index, OS);
      }
    }
  }
};

struct Linking {
  // the version of linking metadata contained in this section (varuint32)
  const uint32_t Version = LinkingVersion;
  // TODO ensure that symbol tables are in front of init func
  std::vector<std::unique_ptr<Subsection>> Subsections;
};
} // namespace watever
