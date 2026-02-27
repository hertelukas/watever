; RUN: watever %s -o %t.o
; RUN: /opt/wasi-sdk/bin/clang %t.o -o %t.wasm
; RUN: wasmtime %t.wasm | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-wasip2"

declare i32 @printf(ptr, ...)

@data = global [3 x i32] [i32 10, i32 20, i32 30]
@format = private constant [4 x i8] c"%d\0A\00"

define i32 @__main_argc_argv(i32 %0, ptr %1) {
entry:
  br label %bb

bb:
  ; Needed to prevent const expressions
  %base = phi ptr [ @data, %entry ]

  ; Points to 20
  %gep1 = getelementptr i32, ptr %base, i32 1

  ; Dependent GEP, points to 30
  %gep2 = getelementptr i32, ptr %gep1, i32 1

  ; Load that absorbs %gep1 into an offset
  %val1 = load i32, ptr %gep1, align 4

  ; 4. Select forces %gep2 (and thus %gep1) to be materialized
  %cmp = icmp sgt i32 %val1, 0
  %sel = select i1 %cmp, ptr %gep2, ptr null

  ; 5. Load from the materialized pointer
  %val2 = load i32, ptr %sel, align 4

  %res = add i32 %val1, %val2

  call i32 (ptr, ...) @printf(ptr @format, i32 %res)
  ret i32 0
}

; CHECK: 50