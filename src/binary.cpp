#include "watever/binary.h"

namespace watever {

void BinaryWriter::write(const Module &) {
  writeMagic();
  writeVersion();
}

} /* namespace watever */
