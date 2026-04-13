// RUN: /opt/wasi-sdk/bin/clang++ -c -emit-llvm --target=wasm32-wasip2 -fwasm-exceptions -mllvm -wasm-use-legacy-eh=false %s -o %t.bc
// RUN: watever %t.bc -o %t.o
// RUN: /opt/wasi-sdk/bin/clang++ %t.o -o %t.wasm -fwasm-exceptions -lunwind
// RUN: wasmtime run -W exceptions=y %t.wasm 2 3 4 5 | FileCheck %s

#include <iostream>

int main(int argc, char **) {
  try {
    if (argc > 1) {
      throw argc;
    }
    return 0;
  } catch (int i) {
    std::cout << "Caught exception: " << i << "\n";
    return 0;
  }
  return 0;
}

// CHECK: Caught exception: 5
