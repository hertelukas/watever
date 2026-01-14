; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-unknown"

declare void @llvm.memmove.p0.p0.i32(ptr writeonly captures(none), ptr readonly captures(none), i32, i1 immarg)

define void @memcpy_i32(ptr %dest, ptr %src, i32 %len) {
; CHECK-LABEL: $memcpy_i32
; CHECK: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: local.get 2
; CHECK-NEXT: memory.copy
  call void @llvm.memmove.p0.p0.i32(ptr %dest, ptr %src, i32 %len, i1 false)
  ret void
}