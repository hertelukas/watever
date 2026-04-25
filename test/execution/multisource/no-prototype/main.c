// RUN: /opt/wasi-sdk/bin/clang -S -emit-llvm --target=wasm32-wasip2 -I %S/inputs %s -o %t.ll
// RUN: /opt/wasi-sdk/bin/clang -S -emit-llvm --target=wasm32-wasip2 -I %S/inputs %S/inputs/foo.c -o %t-foo.ll
// RUN: watever %t.ll -o %t.o
// RUN: watever %t-foo.ll -o %t-foo.o
// RUN: /opt/wasi-sdk/bin/clang %t.o %t-foo.o -o %t.wasm
// RUN: wasmtime %t.wasm | FileCheck %s

#include "foo.h"

int main(int argc, char **argv) {
  (void)argc;
  (void)argv;

  foo();
  return 0;
}

// CHECK: Hello!
