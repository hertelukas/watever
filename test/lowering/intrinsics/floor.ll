; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-unknown"

declare float @llvm.floor.f32(float %val)
declare double @llvm.floor.f64(double %val)

define float @floor_float(float %a) {
; CHECK-LABEL: $floor_float
; CHECK: local.get 0
; CHECK-NEXT: f32.floor
  %1 = call float @llvm.floor.f32(float %a)
  ret float %1
}

define double @floor_double(double %a) {
; CHECK-LABEL: $floor_double
; CHECK: local.get 0
; CHECK-NEXT: f64.floor
  %1 = call double @llvm.floor.f64(double %a)
  ret double %1
}

