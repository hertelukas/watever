; RUN: watever -l 0 --legal %s | FileCheck %s

define float @fsub_float_3(float %a, float %b) {
; CHECK-LABEL: @fsub_float_3
; CHECK: fsub float
entry:
  %0 = fsub float %a, 3.0
  ret float %0
}


define float @fsub_float_float(float %a, float %b) {
; CHECK-LABEL: @fsub_float_float
; CHECK: fsub float
entry:
  %0 = fsub float %a, %b
  ret float %0
}

define double @fsub_double_double(double %a, double %b) {
; CHECK-LABEL: @fsub_double_double
; CHECK: fsub double
entry:
  %0 = fsub double %a, %b
  ret double %0
}
