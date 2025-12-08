; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

define float @fneg_float(float %a) {
; CHECK-LABEL: (func $fneg_float {{.*}} (param f32) (result f32)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    f32.neg
entry:
  %0 = fneg float %a
  ret float %0
}

define double @fneg_double(double %a) {
; CHECK-LABEL: (func $fneg_double {{.*}} (param f64) (result f64)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    f64.neg
entry:
  %0 = fneg double %a
  ret double %0
}
