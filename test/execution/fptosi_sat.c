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

// Directly declare LLVM intrinsics to force Clang to emit them
int32_t fptosi_sat_i32_f32(float) __asm__("llvm.fptosi.sat.i32.f32");
int64_t fptosi_sat_i64_f64(double) __asm__("llvm.fptosi.sat.i64.f64");
int16_t fptosi_sat_i16_f64(double) __asm__("llvm.fptosi.sat.i16.f64");

// Clang's _BitInt allows us to easily test non-power-of-two bitwidths like i48
_BitInt(48) fptosi_sat_i48_f64(double) __asm__("llvm.fptosi.sat.i48.f64");

int main(void) {
  float nan_f32 = NAN;
  double nan_f64 = (double)NAN;

  printf("--- i32 (f32) ---\n");
  printf("normal: %d\n", fptosi_sat_i32_f32(42.9f));
  printf("pos_of: %d\n", fptosi_sat_i32_f32(3000000000.0f));
  printf("neg_of: %d\n", fptosi_sat_i32_f32(-3000000000.0f));
  printf("nan   : %d\n", fptosi_sat_i32_f32(nan_f32));

  printf("--- i64 (f64) ---\n");
  printf("normal: %lld\n", (long long)fptosi_sat_i64_f64(-123.45));
  printf("pos_of: %lld\n", (long long)fptosi_sat_i64_f64(1e20));
  printf("neg_of: %lld\n", (long long)fptosi_sat_i64_f64(-1e20));
  printf("nan   : %lld\n", (long long)fptosi_sat_i64_f64(nan_f64));

  printf("--- i16 (f64) ---\n");
  printf("normal: %d\n", (int)fptosi_sat_i16_f64(100.5));
  printf("pos_of: %d\n", (int)fptosi_sat_i16_f64(40000.0));
  printf("neg_of: %d\n", (int)fptosi_sat_i16_f64(-40000.0));
  printf("nan   : %d\n", (int)fptosi_sat_i16_f64(nan_f64));

  printf("--- i48 (f64) ---\n");
  printf("normal: %lld\n", (long long)fptosi_sat_i48_f64(123456789.0));
  printf("pos_of: %lld\n", (long long)fptosi_sat_i48_f64(1e15)); 
  printf("neg_of: %lld\n", (long long)fptosi_sat_i48_f64(-1e15));
  printf("nan   : %lld\n", (long long)fptosi_sat_i48_f64(nan_f64));

  return 0;
}

// CHECK:      --- i32 (f32) ---
// CHECK-NEXT: normal: 42
// CHECK-NEXT: pos_of: 2147483647
// CHECK-NEXT: neg_of: -2147483648
// CHECK-NEXT: nan   : 0
// CHECK-NEXT: --- i64 (f64) ---
// CHECK-NEXT: normal: -123
// CHECK-NEXT: pos_of: 9223372036854775807
// CHECK-NEXT: neg_of: -9223372036854775808
// CHECK-NEXT: nan   : 0
// CHECK-NEXT: --- i16 (f64) ---
// CHECK-NEXT: normal: 100
// CHECK-NEXT: pos_of: 32767
// CHECK-NEXT: neg_of: -32768
// CHECK-NEXT: nan   : 0
// CHECK-NEXT: --- i48 (f64) ---
// CHECK-NEXT: normal: 123456789
// CHECK-NEXT: pos_of: 140737488355327
// CHECK-NEXT: neg_of: -140737488355328
// CHECK-NEXT: nan   : 0
