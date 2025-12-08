; RUN: watever -l 0 --legal %s | FileCheck %s

define float @fneg_float(float %a) {
; CHECK-LABEL: @fneg_float
; CHECK: fneg float
entry:
  %0 = fneg float %a
  ret float %0
}

define double @fneg_double(double %a) {
; CHECK-LABEL: @fneg_double
; CHECK: fneg double
entry:
  %0 = fneg double %a
  ret double %0
}
