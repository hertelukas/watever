; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

define float @i32_to_float(i32 %a) {
; CHECK-LABEL: i32_to_float
; CHECK-NEXT: local.get 0
; CHECK-NEXT: f32.reinterpret_i32
  %1 = bitcast i32 %a to float
  ret float %1
}

define i32 @float_to_i32(float %a) {
; CHECK-LABEL: float_to_i32
; CHECK-NEXT: local.get 0
; CHECK-NEXT: i32.reinterpret_f32
  %1 = bitcast float %a to i32
  ret i32 %1
}

define double @i64_to_double(i64 %a) {
; CHECK-LABEL: i64_to_double
; CHECK-NEXT: local.get 0
; CHECK-NEXT: f64.reinterpret_i64
  %1 = bitcast i64 %a to double
  ret double %1
}

define i64 @double_to_i64(double %a) {
; CHECK-LABEL: double_to_i64
; CHECK-NEXT: local.get 0
; CHECK-NEXT: i64.reinterpret_f64
  %1 = bitcast double %a to i64
  ret i64 %1
}