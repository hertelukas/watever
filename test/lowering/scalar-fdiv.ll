; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

define float @fdiv_float_float(float %a, float %b) {
; CHECK-LABEL: (func $fdiv_float_float {{.*}} (param f32 f32) (result f32)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    f32.div
entry:
  %0 = fdiv float %a, %b
  ret float %0
}

define float @fdiv_float_const(float %a) {
; CHECK-LABEL: (func $fdiv_float_const {{.*}} (param f32) (result f32)
; CHECK-NEXT:    f32.const {{.*}} (;=13.5;)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    f32.div
entry:
  %0 = fdiv float 13.5, %a
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

define double @fdiv_double_const(double %a) {
; CHECK-LABEL: (func $fdiv_double_const {{.*}} (param f64) (result f64)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    f64.const {{.*}} (;=133.7;)
; CHECK-NEXT:    f64.div
entry:
  %0 = fdiv double %a, 133.7
  ret double %0
}
