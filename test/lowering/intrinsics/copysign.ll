; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

define float @copysign_f32(float %a, float %b) {
; CHECK-LABEL: copysign_f32
; CHECK-NEXT: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: f32.copysign
  %res = call float @llvm.copysign.f32(float %a, float %b)
  ret float %res
}

define double @copysign_f64(double %a, double %b) {
; CHECK-LABEL: copysign_f64
; CHECK-NEXT: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: f64.copysign
  %res = call double @llvm.copysign.f64(double %a, double %b)
  ret double %res
}