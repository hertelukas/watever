; RUN: watever %s -o %t.o
; RUN: /opt/wasi-sdk/bin/clang %t.o -o %t.wasm
; RUN: wasmtime %t.wasm | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-wasip2"

declare i32 @printf(ptr, ...)

@data = global i8 255
@format = private constant [4 x i8] c"%d\0A\00"

define i32 @__main_argc_argv(i32 noundef %0, ptr noundef readnone captures(none) %1) local_unnamed_addr #0 {
entry:
  %val = load i8, ptr @data
  %lt_zero = icmp slt i8 %val, 0
  call i32 (ptr, ...) @printf(ptr @format, i1 %lt_zero)
  ret i32 0
}

; CHECK: 1