// RUN: /opt/wasi-sdk/bin/clang++ -S -emit-llvm --target=wasm32-wasip2 -fno-exceptions %s -o %t.ll
// RUN: watever %t.ll -o %t.o
// RUN: /opt/wasi-sdk/bin/clang++ %t.o -o %t.wasm
// RUN: wasmtime %t.wasm 21 | FileCheck %s

// RUN: /opt/wasi-sdk/bin/clang++ -S -emit-llvm --target=wasm32-wasip2 -fno-exceptions -O3 %s -o %t.O3.ll
// RUN: watever %t.O3.ll -o %t.O3.o
// RUN: /opt/wasi-sdk/bin/clang++ %t.O3.o -o %t.O3.wasm
// RUN: wasmtime %t.O3.wasm | FileCheck %s

#include <stdio.h>

struct B { int i, j; };
struct D : public B {};
int D::*di = &D::i;
int D::*dj = &D::j;

int main() {
  D d;
  d.i = d.j = 0;
  d.*di = 4;
  d.*dj = 7;

  printf("%d %d\n", d.i, d.j);

  return 0;
}

// CHECK: 4 7
