; RUN: watever %s -o %t.o
; RUN: /opt/wasi-sdk/bin/clang %t.o -o %t.wasm
; RUN: wasmtime %t.wasm | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-wasip2"

declare i32 @printf(ptr, ...)

@format = private constant [4 x i8] c"%d\0A\00"

define i32 @__main_argc_argv(i32 noundef %0, ptr noundef readnone captures(none) %1) local_unnamed_addr #0 {
entry:
  %2 = alloca i32
  store i32 5, ptr %2
  %3 = load i32, ptr %2
  store i32 7, ptr %2
  call i32 (ptr, ...) @printf(ptr @format, i32 %3)
  ret i32 0
}

; CHECK: 5