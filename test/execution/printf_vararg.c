// RUN: /opt/wasi-sdk/bin/clang -S -emit-llvm --target=wasm32-wasip2 %s -o %t.ll
// RUN: watever %t.ll -o %t.o
// RUN: /opt/wasi-sdk/bin/clang %t.o -o %t.wasm
// RUN: wasmtime %t.wasm Hello | FileCheck %s

// RUN: /opt/wasi-sdk/bin/clang -S -emit-llvm --target=wasm32-wasip2 -O3 %s -o %t.O3.ll
// RUN: watever %t.O3.ll -o %t.O3.o
// RUN: /opt/wasi-sdk/bin/clang %t.O3.o -o %t.O3.wasm
// RUN: wasmtime %t.O3.wasm Hello | FileCheck %s

#include <stdio.h>

int main(int argc, char **argv) {
  printf("Received %d arguments, last one is \"%s\"\n", argc, argv[argc - 1]);
}

// CHECK: Received 2 arguments, last one is "Hello"
