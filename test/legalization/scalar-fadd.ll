; RUN: watever -l 0 --legal %s | FileCheck %s

define float @fadd_float_3(float %a, float %b) {
; CHECK-LABEL: @fadd_float_3
; CHECK: fadd float
entry:
  %0 = fadd float %a, 3.0
  ret float %0
}


define float @fadd_float_float(float %a, float %b) {
; CHECK-LABEL: @fadd_float_float
; CHECK: fadd float
entry:
  %0 = fadd float %a, %b
  ret float %0
}

define double @fadd_double_double(double %a, double %b) {
; CHECK-LABEL: @fadd_double_double
; CHECK: fadd double
entry:
  %0 = fadd double %a, %b
  ret double %0
}
