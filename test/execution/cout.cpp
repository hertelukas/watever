// RUN: /opt/wasi-sdk/bin/clang++ -S -emit-llvm --target=wasm32-wasip2 -fno-exceptions %s -o %t.ll 
// RUN: watever %t.ll -o %t.o
// RUN: /opt/wasi-sdk/bin/clang++ %t.o -o %t.wasm
// RUN: wasmtime %t.wasm | FileCheck %s

#include <iostream>
int main(int argc, char **argv) { std::cout << " Hello World!\n"; }

// CHECK: Hello World!
