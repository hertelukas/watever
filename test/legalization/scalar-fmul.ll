; RUN: watever -l 0 --legal %s | FileCheck %s

define float @fmul_float_3(float %a, float %b) {
; CHECK-LABEL: @fmul_float_3
; CHECK: fmul float
entry:
  %0 = fmul float %a, 3.0
  ret float %0
}


define float @fmul_float_float(float %a, float %b) {
; CHECK-LABEL: @fmul_float_float
; CHECK: fmul float
entry:
  %0 = fmul float %a, %b
  ret float %0
}

define double @fmul_double_double(double %a, double %b) {
; CHECK-LABEL: @fmul_double_double
; CHECK: fmul double
entry:
  %0 = fmul double %a, %b
  ret double %0
}
