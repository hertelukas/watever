; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

define float @frem_float_float(float %a, float %b) {
; CHECK-LABEL: (func $frem_float_float {{.*}} (param f32 f32) (result f32)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    call
entry:
  %0 = frem float %a, %b
  ret float %0
}

define double @frem_double_double(double %a, double %b) {
; CHECK-LABEL: (func $frem_double_double {{.*}} (param f64 f64) (result f64)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    call
entry:
  %0 = frem double %a, %b
  ret double %0
}
