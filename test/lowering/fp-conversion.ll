; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

define double @float_to_double(float %a) {
; CHECK-LABEL: float_to_double
; CHECK: local.get 0
; CHECK-NEXT: f64.promote_f32
  %1 = fpext float %a to double
  ret double %1
}


define float @double_to_float(double %a) {
; CHECK-LABEL: double_to_float
; CHECK: local.get 0
; CHECK-NEXT: f32.demote_f64
  %1 = fptrunc double %a to float
  ret float %1
}
