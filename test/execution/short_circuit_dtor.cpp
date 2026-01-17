// RUN: /opt/wasi-sdk/bin/clang++ -S -emit-llvm --target=wasm32-wasip2 -fno-exceptions %s -o %t.ll
// RUN: watever %t.ll -o %t.o
// RUN: /opt/wasi-sdk/bin/clang++ %t.o -o %t.wasm
// RUN: wasmtime %t.wasm 21 | FileCheck %s

// RUN: /opt/wasi-sdk/bin/clang++ -S -emit-llvm --target=wasm32-wasip2 -fno-exceptions -O3 %s -o %t.O3.ll
// RUN: watever %t.O3.ll -o %t.O3.o
// RUN: /opt/wasi-sdk/bin/clang++ %t.O3.o -o %t.O3.wasm
// RUN: wasmtime %t.O3.wasm | FileCheck %s

#include <stdio.h>

struct X {
  int Val;
  X(int i) : Val(i) { printf("X::X(%d)\n", Val); }
  ~X() { printf("X::~X(%d)\n", Val); }
};

bool foo(const X &) { return true; }
void bar() {}
int main() {
  if ((foo(1) || foo(2)))
    bar();
  return 0;
}

// CHECK: X::X(1)
// CHECK-NEXT: X::~X(1)
