#include "watever/binary.h"
#include "watever/ir.h"
#include "watever/utils.h"
#include <llvm/Support/LEB128.h>
#include <variant>

namespace watever {

template <class... Ts> struct Overloaded : Ts... {
  using Ts::operator()...;
};

void BinaryWriter::writeTypes() {
  // Prepare the content of the section
  llvm::SmallVector<char> Content;
  llvm::raw_svector_ostream ContentOS(Content);
  // list(type)
  llvm::encodeULEB128(Mod.Types.size(), ContentOS);

  uint32_t TypeIndex{};
  for (auto &[_, value] : Mod.Types) {
    value->Index = TypeIndex++;

    std::visit(
        Overloaded{[&](StructType &) { WATEVER_TODO("encode structype"); },
                   [&](ArrayType &) { WATEVER_TODO("encode array type"); },
                   [&](FuncType &FT) {
                     ContentOS << Type::Enum::Func;
                     // Encode args
                     llvm::encodeULEB128(FT.Args.size(), ContentOS);
                     for (const auto &Arg : FT.Args) {
                       ContentOS << Arg;
                     }
                     // Encode results
                     llvm::encodeULEB128(FT.Results.size(), ContentOS);
                     for (const auto &Res : FT.Results) {
                       ContentOS << Res;
                     }
                   }

        },
        value->Composite);
    // TODO support rec types
  }

  OS << static_cast<uint8_t>(Section::Type);
  llvm::encodeULEB128(Content.size(), OS);
  OS << ContentOS.str();
}

void BinaryWriter::write() {
  writeMagic();
  writeVersion();

  writeTypes();
}

} /* namespace watever */
