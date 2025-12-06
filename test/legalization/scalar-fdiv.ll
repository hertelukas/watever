; RUN: watever -l 0 --legal %s | FileCheck %s

define float @fdiv_float_3(float %a, float %b) {
; CHECK-LABEL: @fdiv_float_3
; CHECK: fdiv float
entry:
  %0 = fdiv float %a, 3.0
  ret float %0
}


define float @fdiv_float_float(float %a, float %b) {
; CHECK-LABEL: @fdiv_float_float
; CHECK: fdiv float
entry:
  %0 = fdiv float %a, %b
  ret float %0
}

define double @fdiv_double_double(double %a, double %b) {
; CHECK-LABEL: @fdiv_double_double
; CHECK: fdiv double
entry:
  %0 = fdiv double %a, %b
  ret double %0
}
