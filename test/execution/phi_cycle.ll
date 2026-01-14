; RUN: watever %s -o %t.o
; RUN: /opt/wasi-sdk/bin/clang %t.o -o %t.wasm
; RUN: wasmtime %t.wasm | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-wasip2"

declare i32 @printf(ptr, ...)

@format = private constant [4 x i8] c"%d\0A\00"

define void @test_phi_cycle() {
entry:
  br label %loop

loop:
  %a = phi i32 [ 100, %entry ], [ %b, %loop ]
  %b = phi i32 [ 200, %entry ], [ %a, %loop ]
  %i = phi i32 [ 0, %entry ], [ %next_i, %loop ]

  %next_i = add i32 %i, 1
  %cond = icmp ult i32 %next_i, 2
  br i1 %cond, label %loop, label %exit

exit:
  call i32 (ptr, ...) @printf(ptr @format, i32 %a)
  call i32 (ptr, ...) @printf(ptr @format, i32 %b)
  ret void
}

define i32 @__main_argc_argv(i32 noundef %0, ptr noundef readnone captures(none) %1) local_unnamed_addr #0 {
  call void @test_phi_cycle()

  ret i32 0
}

; CHECK: 200
; CHECK-NEXT: 100
