; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

define float @max_float_constant(float %a) {
; CHECK-LABEL: $max_float_constant
; CHECK-NEXT: local.get 0
; CHECK-NEXT: f32.const 0x1p+2
; CHECK-NEXT: f32.max
  %res = call float @llvm.maximum.f32(float %a, float 4.0)
  ret float %res
}

define float @max_float(float %a, float %b) {
; CHECK-LABEL: $max_float
; CHECK-NEXT: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: f32.max
  %res = call float @llvm.maximum.f32(float %a, float %b)
  ret float %res
}

define float @min_float_constant(float %a) {
; CHECK-LABEL: $min_float_constant
; CHECK-NEXT: local.get 0
; CHECK-NEXT: f32.const 0x1p+2
; CHECK-NEXT: f32.min
  %res = call float @llvm.minimum.f32(float %a, float 4.0)
  ret float %res
}

define float @min_float(float %a, float %b) {
; CHECK-LABEL: $min_float
; CHECK-NEXT: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: f32.min
  %res = call float @llvm.minimum.f32(float %a, float %b)
  ret float %res
}

define double @max_double_constant(double %a) {
; CHECK-LABEL: $max_double_constant
; CHECK-NEXT: local.get 0
; CHECK-NEXT: f64.const 0x1p+2
; CHECK-NEXT: f64.max
  %res = call double @llvm.maximum.f64(double %a, double 4.0)
  ret double %res
}

define double @max_double(double %a, double %b) {
; CHECK-LABEL: $max_double
; CHECK-NEXT: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: f64.max
  %res = call double @llvm.maximum.f64(double %a, double %b)
  ret double %res
}

define double @min_double_constant(double %a) {
; CHECK-LABEL: $min_double_constant
; CHECK-NEXT: local.get 0
; CHECK-NEXT: f64.const 0x1p+2
; CHECK-NEXT: f64.min
  %res = call double @llvm.minimum.f64(double %a, double 4.0)
  ret double %res
}

define double @min_double(double %a, double %b) {
; CHECK-LABEL: $min_double
; CHECK-NEXT: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: f64.min
  %res = call double @llvm.minimum.f64(double %a, double %b)
  ret double %res
}