#include "watever/binary.h"

namespace watever {

void BinaryWriter::write(const Module &Mod) {
  writeMagic();
  writeVersion();
}

} /* namespace watever */
