// RUN: /opt/wasi-sdk/bin/clang++ -S -emit-llvm --target=wasm32-wasip2 -fno-exceptions %s -o %t.ll
// RUN: watever %t.ll -o %t.o
// RUN: /opt/wasi-sdk/bin/clang++ %t.o -o %t.wasm
// RUN: wasmtime %t.wasm 21 | FileCheck %s

// RUN: /opt/wasi-sdk/bin/clang++ -S -emit-llvm --target=wasm32-wasip2 -fno-exceptions -O3 %s -o %t.O3.ll
// RUN: watever %t.O3.ll -o %t.O3.o
// RUN: /opt/wasi-sdk/bin/clang++ %t.O3.o -o %t.O3.wasm
// RUN: wasmtime %t.O3.wasm | FileCheck %s

#include <stdio.h>

struct B { 
  int X;
  void i() {
    printf("i, %d\n", X);
  }
  void j() {
    printf("j, %d\n", X);
  }
};

void foo(int V, void (B::*Fn)()) {
   B b;  b.X = V;
   (b.*Fn)();
}

int main() {
	foo(4, &B::i);
	foo(6, &B::j);
	foo(-1, &B::i);
	return 0;
}

// CHECK: i, 4
// CHECK-NEXT: j, 6
// CHECK-NEXT: i, -1
