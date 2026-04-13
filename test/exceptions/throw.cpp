// RUN: /opt/wasi-sdk/bin/clang++ -S -emit-llvm --target=wasm32-wasip2 -fwasm-exceptions %s -o %t.ll
// RUN: watever %t.ll -o %t.o
// RUN: /opt/wasi-sdk/bin/clang++ %t.o -o %t.wasm -fwasm-exceptions -lunwind
// RUN: not wasmtime run -W exceptions=y %t.wasm 2>&1 | FileCheck %s

int main(int argc, char **argv) { throw 1; }

// CHECK: thrown Wasm exception
