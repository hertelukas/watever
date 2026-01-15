; RUN: watever -l 0 --legal %s | FileCheck %s

define double @float_to_double(float %a) {
; CHECK-LABEL: float_to_double
; CHECK: fpext float %a to double
  %1 = fpext float %a to double
  ret double %1
}


define float @double_to_float(double %a) {
; CHECK-LABEL: double_to_float
; CHECK: fptrunc double %a to float
  %1 = fptrunc double %a to float
  ret float %1
}
