; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

; TODO test constants

define float @fdiv_float_float(float %a, float %b) {
; CHECK-LABEL: (func $fdiv_float_float {{.*}} (param f32 f32) (result f32)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    f32.div
entry:
  %0 = fdiv float %a, %b
  ret float %0
}

define double @fdiv_double_double(double %a, double %b) {
; CHECK-LABEL: (func $fdiv_double_double {{.*}} (param f64 f64) (result f64)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    f64.div
entry:
  %0 = fdiv double %a, %b
  ret double %0
}
