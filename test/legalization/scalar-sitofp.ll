; RUN: watever -l 0 %s --legal | FileCheck %s

define float @i8_to_float(i8 %a) {
; CHECK-LABEL: @i8_to_float
; CHECK: sext i8 {{.*}} to i32
; CHECK: sitofp i32 {{.*}} to float
  %1 = sitofp i8 %a to float
  ret float %1
}

define double @i8_to_double(i8 %a) {
; CHECK-LABEL: @i8_to_double
; CHECK: sext i8 {{.*}} to i32
; CHECK: sitofp i32 {{.*}} to double
  %1 = sitofp i8 %a to double
  ret double %1
}

define float @i32_to_float(i32 %a) {
; CHECK-LABEL: @i32_to_float
; CHECK: sitofp i32 {{.*}} to float
  %1 = sitofp i32 %a to float
  ret float %1
}

define double @i32_to_double(i32 %a) {
; CHECK-LABEL: @i32_to_double
; CHECK: sitofp i32 {{.*}} to double
  %1 = sitofp i32 %a to double
  ret double %1
}

define float @i51_to_float(i51 %a) {
; CHECK-LABEL: @i51_to_float
; CHECK: shl
; CHECK: ashr i64 {{.*}}, 13
; CHEK: sitofp i64 {{.*}} to float
  %1 = sitofp i51 %a to float
  ret float %1
}

define double @i51_to_double(i51 %a) {
; CHECK-LABEL: @i51_to_double
; CHECK: shl
; CHECK: ashr i64 {{.*}}, 13
; CHEK: sitofp i64 {{.*}} to double
  %1 = sitofp i51 %a to double
  ret double %1
}


define float @i64_to_float(i64 %a) {
; CHECK-LABEL: @i64_to_float
; CHECK: sitofp i64 {{.*}} to float
  %1 = sitofp i64 %a to float
  ret float %1
}

define double @i64_to_double(i64 %a) {
; CHECK-LABEL: @i64_to_double
; CHECK: sitofp i64 {{.*}} to double
  %1 = sitofp i64 %a to double
  ret double %1
}
