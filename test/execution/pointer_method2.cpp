// RUN: /opt/wasi-sdk/bin/clang++ -S -emit-llvm --target=wasm32-wasip2 -fno-exceptions %s -o %t.ll
// RUN: watever %t.ll -o %t.o
// RUN: /opt/wasi-sdk/bin/clang++ %t.o -o %t.wasm
// RUN: wasmtime %t.wasm 21 | FileCheck %s

// RUN: /opt/wasi-sdk/bin/clang++ -S -emit-llvm --target=wasm32-wasip2 -fno-exceptions -O3 %s -o %t.O3.ll
// RUN: watever %t.O3.ll -o %t.O3.o
// RUN: /opt/wasi-sdk/bin/clang++ %t.O3.o -o %t.O3.wasm
// RUN: wasmtime %t.O3.wasm | FileCheck %s

#include <stdio.h>

struct A {
  int a;
  virtual void foo() = 0;
  void bar() { printf("A::bar(): a=%x\n", a); }
};

struct B {
  int b;
  virtual void foo() = 0;
  void bar() { printf("B::bar(): b=%x\n", b); }
};

struct C : A, B {
  int c;
  virtual void foo() { printf("C::foo(), c=%x\n", c); }
  void bar() { printf("C::bar(), c=%x\n", c); }
};

template <class T> void invoke(C &c, void (T::*fn)()) {
  (c.*fn)();
}

int main() {
  C c;
  c.a = 0xff;
  c.b = 0xf0f;
  c.c = 0xf00f;

  invoke(c, &A::foo);
  invoke(c, &A::bar);
  invoke(c, &B::foo);
  invoke(c, &B::bar);
  invoke(c, &C::foo);
  invoke(c, &C::bar);
}

// CHECK: C::foo(), c=f00f
// CHECK-NEXT: A::bar(): a=ff
// CHECK-NEXT: C::foo(), c=f00f
// CHECK-NEXT: B::bar(): b=f0f
// CHECK-NEXT: C::foo(), c=f00f
// CHECK-NEXT: C::bar(), c=f00f
