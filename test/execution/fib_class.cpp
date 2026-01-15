// RUN: /opt/wasi-sdk/bin/clang++ -S -emit-llvm --target=wasm32-wasip2 -fno-exceptions %s -o %t.ll
// RUN: watever %t.ll -o %t.o
// RUN: /opt/wasi-sdk/bin/clang++ %t.o -o %t.wasm
// RUN: wasmtime %t.wasm 21 | FileCheck %s

// RUN: /opt/wasi-sdk/bin/clang++ -S -emit-llvm --target=wasm32-wasip2 -fno-exceptions -O3 -fno-unroll-loops %s -o %t.O3.ll
// RUN: watever %t.O3.ll -o %t.O3.o
// RUN: /opt/wasi-sdk/bin/clang++ %t.O3.o -o %t.O3.wasm
// RUN: wasmtime %t.O3.wasm 21 | FileCheck %s

#include <iostream>
#include <string>

struct Data {
  int a;
  int b;

  Data(int a, int b) : a{a}, b{b} {}
};

static int fib(int n, Data d) {
  if (n == 0) {
    return d.a + d.b;
  }
  auto D = Data(d.b, d.a + d.b);
  return fib(n - 1, D);
}

int main(int argc, char **argv) {
  if (argc < 2)
    return 1;
  int num = std::stoi(argv[1]);
  auto D = Data(0, 1);
  int res = fib(num - 2, D);
  std::cout << "The " << num << "th fibonacci number is " << res << "\n";
}

// CHECK: The 21th fibonacci number is 10946
