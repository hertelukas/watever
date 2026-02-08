; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-unknown"

declare void @llvm.memset.p0.i32(ptr writeonly captures(none), i8, i32, i1 immarg)
declare void @llvm.memset.p0.i64(ptr writeonly captures(none), i8, i64, i1 immarg)

; TODO should skip if len == 0
define void @memset_i32(ptr %dest, i8 %val, i32 %len) {
; CHECK-LABEL: $memset_i32
; CHECK: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: local.get 2
; CHECK-NEXT: memory.fill
  call void @llvm.memset.p0.i32(ptr %dest, i8 %val, i32 %len, i1 false)
  ret void
}

define void @memset_i32_const(ptr %dest, i32 %len) {
; CHECK-LABEL: $memset_i32
; CHECK: local.get 0
; CHECK-NEXT: i32.const 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: memory.fill
  call void @llvm.memset.p0.i32(ptr %dest, i8 0, i32 %len, i1 false)
  ret void
}

define void @memset_i64(ptr %dest, i8 %val, i64 %len) {
; CHECK-LABEL: $memset_i64
; CHECK: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: local.get 2
; CHECK-NEXT: i32.wrap_i64
; CHECK-NEXT: memory.fill
  call void @llvm.memset.p0.i64(ptr %dest, i8 %val, i64 %len, i1 false)
  ret void
}
