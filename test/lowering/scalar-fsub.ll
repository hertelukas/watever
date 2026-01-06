; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

define float @fsub_float_float(float %a, float %b) {
; CHECK-LABEL: (func $fsub_float_float {{.*}} (param f32 f32) (result f32)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    f32.sub
entry:
  %0 = fsub float %a, %b
  ret float %0
}

define float @fsub_float_const(float %a) {
; CHECK-LABEL: (func $fsub_float_const {{.*}} (param f32) (result f32)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    f32.const {{.*}} (;=2;)
; CHECK-NEXT:    f32.sub
entry:
  %0 = fsub float %a, 2.0
  ret float %0
}

define double @fsub_double_double(double %a, double %b) {
; CHECK-LABEL: (func $fsub_double_double {{.*}} (param f64 f64) (result f64)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    f64.sub
entry:
  %0 = fsub double %a, %b
  ret double %0
}

define double @fsub_double_const(double %a) {
; CHECK-LABEL: (func $fsub_double_const {{.*}} (param f64) (result f64)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    f64.const {{.*}} (;=-3.5;)
; CHECK-NEXT:    f64.sub
entry:
  %0 = fsub double %a, -3.5
  ret double %0
}
