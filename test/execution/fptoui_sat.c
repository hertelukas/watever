// RUN: /opt/wasi-sdk/bin/clang -S -emit-llvm --target=wasm32-wasip2 %s -o %t.ll
// RUN: watever %t.ll -o %t.o
// RUN: /opt/wasi-sdk/bin/clang %t.o -o %t.wasm
// RUN: wasmtime %t.wasm | FileCheck %s

// RUN: /opt/wasi-sdk/bin/clang -S -emit-llvm --target=wasm32-wasip2 -O3 %s -o %t.O3.ll
// RUN: watever %t.O3.ll -o %t.O3.o
// RUN: /opt/wasi-sdk/bin/clang %t.O3.o -o %t.O3.wasm
// RUN: wasmtime %t.O3.wasm | FileCheck %s

#include <stdio.h>
#include <stdint.h>
#include <math.h>

// Explicitly use unsigned types for unsigned saturation
uint32_t fptoui_sat_i32_f32(float) __asm__("llvm.fptoui.sat.i32.f32");
uint64_t fptoui_sat_i64_f64(double) __asm__("llvm.fptoui.sat.i64.f64");
uint16_t fptoui_sat_i16_f64(double) __asm__("llvm.fptoui.sat.i16.f64");

// Use unsigned _BitInt for custom bitwidths
unsigned _BitInt(48) fptoui_sat_i48_f64(double) __asm__("llvm.fptoui.sat.i48.f64");

int main(void) {
  printf("--- i32 (f32) ---\n");
  printf("normal: %u\n", fptoui_sat_i32_f32(42.9f));
  printf("pos_of: %u\n", fptoui_sat_i32_f32(5000000000.0f)); // > 4.29 billion
  printf("neg_of: %u\n", fptoui_sat_i32_f32(-1.0f));
  printf("inf   : %u\n", fptoui_sat_i32_f32(INFINITY));
  printf("ninf  : %u\n", fptoui_sat_i32_f32(-INFINITY));
  printf("mzero : %u\n", fptoui_sat_i32_f32(-0.0f));
  printf("nan   : %u\n", fptoui_sat_i32_f32(NAN));

  printf("--- i64 (f64) ---\n");
  printf("normal: %llu\n", (unsigned long long)fptoui_sat_i64_f64(123.45));
  printf("pos_of: %llu\n", (unsigned long long)fptoui_sat_i64_f64(1e20));
  printf("neg_of: %llu\n", (unsigned long long)fptoui_sat_i64_f64(-1e20));
  printf("inf   : %llu\n", (unsigned long long)fptoui_sat_i64_f64(INFINITY));
  printf("nan   : %llu\n", (unsigned long long)fptoui_sat_i64_f64(NAN));

  printf("--- i16 (f64) ---\n");
  printf("normal: %u\n", (unsigned)fptoui_sat_i16_f64(100.5));
  printf("pos_of: %u\n", (unsigned)fptoui_sat_i16_f64(70000.0)); // > 65535
  printf("exact : %u\n", (unsigned)fptoui_sat_i16_f64(65535.0));
  printf("neg_of: %u\n", (unsigned)fptoui_sat_i16_f64(-40000.0));
  printf("nan   : %u\n", (unsigned)fptoui_sat_i16_f64(NAN));

  printf("--- i48 (f64) ---\n");
  // Max unsigned i48 is 2^48 - 1 = 281474976710655
  printf("normal: %llu\n", (unsigned long long)fptoui_sat_i48_f64(123456789.0));
  printf("pos_of: %llu\n", (unsigned long long)fptoui_sat_i48_f64(1e15));
  printf("exact : %llu\n", (unsigned long long)fptoui_sat_i48_f64(281474976710655.0));
  printf("neg_of: %llu\n", (unsigned long long)fptoui_sat_i48_f64(-1e15));
  printf("nan   : %llu\n", (unsigned long long)fptoui_sat_i48_f64(NAN));

  return 0;
}

// CHECK:      --- i32 (f32) ---
// CHECK-NEXT: normal: 42
// CHECK-NEXT: pos_of: 4294967295
// CHECK-NEXT: neg_of: 0
// CHECK-NEXT: inf   : 4294967295
// CHECK-NEXT: ninf  : 0
// CHECK-NEXT: mzero : 0
// CHECK-NEXT: nan   : 0
// CHECK-NEXT: --- i64 (f64) ---
// CHECK-NEXT: normal: 123
// CHECK-NEXT: pos_of: 18446744073709551615
// CHECK-NEXT: neg_of: 0
// CHECK-NEXT: inf   : 18446744073709551615
// CHECK-NEXT: nan   : 0
// CHECK-NEXT: --- i16 (f64) ---
// CHECK-NEXT: normal: 100
// CHECK-NEXT: pos_of: 65535
// CHECK-NEXT: exact : 65535
// CHECK-NEXT: neg_of: 0
// CHECK-NEXT: nan   : 0
// CHECK-NEXT: --- i48 (f64) ---
// CHECK-NEXT: normal: 123456789
// CHECK-NEXT: pos_of: 281474976710655
// CHECK-NEXT: exact : 281474976710655
// CHECK-NEXT: neg_of: 0
// CHECK-NEXT: nan   : 0
