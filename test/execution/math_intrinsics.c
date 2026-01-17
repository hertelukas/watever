// RUN: /opt/wasi-sdk/bin/clang -S -emit-llvm --target=wasm32-wasip2 %s -o %t.ll
// RUN: watever %t.ll -o %t.o
// RUN: /opt/wasi-sdk/bin/clang %t.o -o %t.wasm
// RUN: wasmtime %t.wasm | FileCheck %s

// Neded for exp10
#define _GNU_SOURCE
#include <math.h>
#include <stdio.h>

int main(void) {
  printf("sin(%.2f) = %.2f\n", 0.3, sin(0.3));
  printf("cos(%.2f) = %.2f\n", 0.4, cosf(0.4f));
  printf("tan(%.2f) = %.2f\n", 0.5, tan(0.5));
  printf("asin(%.2f) = %.2f\n", 0.6, asinf(0.6f));
  printf("acos(%.2f) = %.2f\n", 0.7, acos(0.7));
  printf("atan(%.2f) = %.2f\n", 0.8, atanf(0.8f));
  printf("sinh(%.2f) = %.2f\n", 0.9, sinh(0.9));
  printf("cosh(%.2f) = %.2f\n", 1.0, coshf(1.0f));
  printf("tanh(%.2f) = %.2f\n", 1.1, tanh(1.1));
  printf("exp(%.2f) = %.2f\n", 1.2, expf(1.2f));
  printf("exp2(%.2f) = %.2f\n", 3.0, exp2(3.0));
  printf("exp10(%.2f) = %.2f\n", 2.0, exp10f(2.0f));
  printf("log(%.2f) = %.2f\n", 10.0, log(10.0));
  printf("log10(%.2f) = %.2f\n", 100.0, log10f(100.0f));
  printf("log2(%.2f) = %.2f\n", 8.0, log2(8.0));
  printf("fabs(%.2f) = %.2f\n", -123.45, fabsf(-123.45f));
  return 0;
}

// CHECK:      sin(0.30) = 0.30
// CHECK-NEXT: cos(0.40) = 0.92
// CHECK-NEXT: tan(0.50) = 0.55
// CHECK-NEXT: asin(0.60) = 0.64
// CHECK-NEXT: acos(0.70) = 0.80
// CHECK-NEXT: atan(0.80) = 0.67
// CHECK-NEXT: sinh(0.90) = 1.03
// CHECK-NEXT: cosh(1.00) = 1.54
// CHECK-NEXT: tanh(1.10) = 0.80
// CHECK-NEXT: exp(1.20) = 3.32
// CHECK-NEXT: exp2(3.00) = 8.00
// CHECK-NEXT: exp10(2.00) = 100.00
// CHECK-NEXT: log(10.00) = 2.30
// CHECK-NEXT: log10(100.00) = 2.00
// CHECK-NEXT: log2(8.00) = 3.00
// CHECK-NEXT: fabs(-123.45) = 123.45
