; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

define float @fmul_float_float(float %a, float %b) {
; CHECK-LABEL: (func $fmul_float_float {{.*}} (param f32 f32) (result f32)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    f32.mul
entry:
  %0 = fmul float %a, %b
  ret float %0
}

define float @fmul_float_const(float %a) {
; CHECK-LABEL: (func $fmul_float_const {{.*}} (param f32) (result f32)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    f32.const {{.*}} (;=2;)
; CHECK-NEXT:    f32.mul
entry:
  %0 = fmul float %a, 2.0
  ret float %0
}

define double @fmul_double_double(double %a, double %b) {
; CHECK-LABEL: (func $fmul_double_double {{.*}} (param f64 f64) (result f64)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    f64.mul
entry:
  %0 = fmul double %a, %b
  ret double %0
}


define double @fmul_double_const(double %a) {
; CHECK-LABEL: (func $fmul_double_const {{.*}} (param f64) (result f64)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    f64.const {{.*}} (;=-3.5;)
; CHECK-NEXT:    f64.mul
entry:
  %0 = fmul double %a, -3.5
  ret double %0
}
