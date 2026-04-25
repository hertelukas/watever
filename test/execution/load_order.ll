; RUN: watever %s -o %t.o
; RUN: /opt/wasi-sdk/bin/clang %t.o -o %t.wasm
; RUN: wasmtime %t.wasm | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-wasip2"

declare i32 @printf(ptr, ...)
@format = private constant [4 x i8] c"%d\0A\00"

define i32 @__main_argc_argv(i32 %0, ptr %1) {
entry:
  %ptr = alloca i32, align 4
  store i32 42, ptr %ptr, align 4

  %val1 = load i32, ptr %ptr, align 4
  %val2 = load i32, ptr %ptr, align 4

  %dummy_ptr = alloca i32, align 4
  store i32 %val2, ptr %dummy_ptr, align 4

  store i32 100, ptr %dummy_ptr, align 4

  call i32 (ptr, ...) @printf(ptr @format, i32 %val1)
  ret i32 0
}

; CHECK: 42
