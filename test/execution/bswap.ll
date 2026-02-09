; RUN: watever %s -o %t.o
; RUN: /opt/wasi-sdk/bin/clang %t.o -o %t.wasm
; RUN: wasmtime %t.wasm | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-wasip2"

declare i32 @llvm.bswap.i32(i32)
declare i64 @llvm.bswap.i64(i64)

declare i32 @printf(ptr, ...)
@formati32 = private constant [6 x i8] c"%08x\0A\00"
@formati64 = private constant [9 x i8] c"%016llx\0A\00"

define i32 @__main_argc_argv(i32 noundef %0, ptr noundef readnone captures(none) %1) local_unnamed_addr #0 {
entry:
; 0x01234567 = 19088743
  %i32 = call i32 @llvm.bswap.i32(i32 19088743)
  call i32 (ptr, ...) @printf(ptr @formati32, i32 %i32)
; 0x0123456789ABCDEF = 81985529216486895
  %i64 = call i64 @llvm.bswap.i64(i64 81985529216486895)
  call i32 (ptr, ...) @printf(ptr @formati64, i64 %i64)
  ret i32 0
}

; CHECK: 67452301
; CHECK: efcdab8967452301