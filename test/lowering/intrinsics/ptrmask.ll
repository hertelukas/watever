; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-unknown"

declare ptr @llvm.ptrmask.p0.i32(ptr %ptr, i32 %mask)

define ptr @ptrmask(ptr %ptr, i32 %mask) {
; CHECK-LABEL: $ptrmask
; CHECK: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: and
  %1 = call ptr @llvm.ptrmask.p0.i32(ptr %ptr, i32 %mask)
  ret ptr %1
}

define ptr @ptrmask_const(ptr %ptr) {
; CHECK-LABEL: $ptrmask_const
; CHECK: local.get 0
; CHECK-NEXT: i32.const -8
; CHECK-NEXT: and
  %1 = call ptr @llvm.ptrmask.p0.i32(ptr %ptr, i32 -8)
  ret ptr %1
}