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

int16_t sadd_sat_i16(int16_t, int16_t) __asm__("llvm.sadd.sat.i16");
int32_t sadd_sat_i32(int32_t, int32_t) __asm__("llvm.sadd.sat.i32");

int16_t ssub_sat_i16(int16_t, int16_t) __asm__("llvm.ssub.sat.i16");
int32_t ssub_sat_i32(int32_t, int32_t) __asm__("llvm.ssub.sat.i32");

uint16_t uadd_sat_i16(uint16_t, uint16_t) __asm__("llvm.uadd.sat.i16");
uint32_t uadd_sat_i32(uint32_t, uint32_t) __asm__("llvm.uadd.sat.i32");

uint16_t usub_sat_i16(uint16_t, uint16_t) __asm__("llvm.usub.sat.i16");
uint32_t usub_sat_i32(uint32_t, uint32_t) __asm__("llvm.usub.sat.i32");

int main(void) {
  printf("--- sadd_sat ---\n");
  // CHECK: --- sadd_sat ---
  // CHECK-NEXT: i16 normal: 30
  // CHECK-NEXT: i16 pos_of: 32767
  // CHECK-NEXT: i16 neg_of: -32768
  printf("i16 normal: %d\n", sadd_sat_i16(10, 20));
  printf("i16 pos_of: %d\n", sadd_sat_i16(30000, 10000));
  printf("i16 neg_of: %d\n", sadd_sat_i16(-30000, -10000));

  // CHECK-NEXT: i32 normal: 300
  // CHECK-NEXT: i32 pos_of: 2147483647
  // CHECK-NEXT: i32 neg_of: -2147483648
  printf("i32 normal: %d\n", sadd_sat_i32(100, 200));
  printf("i32 pos_of: %d\n", sadd_sat_i32(2000000000, 1000000000));
  printf("i32 neg_of: %d\n", sadd_sat_i32(-2000000000, -1000000000));

  printf("--- ssub_sat ---\n");
  // CHECK: --- ssub_sat ---
  // CHECK-NEXT: i16 normal: 10
  // CHECK-NEXT: i16 pos_of: 32767
  // CHECK-NEXT: i16 neg_of: -32768
  printf("i16 normal: %d\n", ssub_sat_i16(20, 10));
  printf("i16 pos_of: %d\n", ssub_sat_i16(30000, -10000));
  printf("i16 neg_of: %d\n", ssub_sat_i16(-30000, 10000));

  // CHECK-NEXT: i32 normal: 100
  // CHECK-NEXT: i32 pos_of: 2147483647
  // CHECK-NEXT: i32 neg_of: -2147483648
  printf("i32 normal: %d\n", ssub_sat_i32(200, 100));
  printf("i32 pos_of: %d\n", ssub_sat_i32(2000000000, -1000000000));
  printf("i32 neg_of: %d\n", ssub_sat_i32(-2000000000, 1000000000));

  printf("--- uadd_sat ---\n");
  // CHECK: --- uadd_sat ---
  // CHECK-NEXT: i16 normal: 30
  // CHECK-NEXT: i16 pos_of: 65535
  printf("i16 normal: %u\n", uadd_sat_i16(10, 20));
  printf("i16 pos_of: %u\n", uadd_sat_i16(60000, 10000));

  // CHECK-NEXT: i32 normal: 300
  // CHECK-NEXT: i32 pos_of: 4294967295
  printf("i32 normal: %u\n", uadd_sat_i32(100, 200));
  printf("i32 pos_of: %u\n", uadd_sat_i32(3000000000, 2000000000));

  printf("--- usub_sat ---\n");
  // CHECK: --- usub_sat ---
  // CHECK-NEXT: i16 normal: 10
  // CHECK-NEXT: i16 neg_of: 0
  printf("i16 normal: %u\n", usub_sat_i16(20, 10));
  printf("i16 neg_of: %u\n", usub_sat_i16(10, 20));

  // CHECK-NEXT: i32 normal: 100
  // CHECK-NEXT: i32 neg_of: 0
  printf("i32 normal: %u\n", usub_sat_i32(200, 100));
  printf("i32 neg_of: %u\n", usub_sat_i32(100, 200));

  return 0;
}
