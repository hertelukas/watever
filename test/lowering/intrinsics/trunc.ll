; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-unknown"

declare float @llvm.trunc.f32(float %val)
declare double @llvm.trunc.f64(double %val)

define float @trunc_float(float %a) {
; CHECK-LABEL: $trunc_float
; CHECK: local.get 0
; CHECK-NEXT: f32.trunc
  %1 = call float @llvm.trunc.f32(float %a)
  ret float %1
}

define double @trunc_double(double %a) {
; CHECK-LABEL: $trunc_double
; CHECK: local.get 0
; CHECK-NEXT: f64.trunc
  %1 = call double @llvm.trunc.f64(double %a)
  ret double %1
}
