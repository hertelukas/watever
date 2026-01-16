; RUN: watever -l 0 %s --legal | FileCheck %s

define float @u8_to_float(i8 %a) {
; CHECK-LABEL: @u8_to_float
; CHECK: and i32 {{.*}}, 255
; CHECK: uitofp i32 {{.*}} to float
  %1 = uitofp i8 %a to float
  ret float %1
}

define double @u8_to_double(i8 %a) {
; CHECK-LABEL: @u8_to_double
; CHECK: and i32 {{.*}}, 255
; CHECK: uitofp i32 {{.*}} to double
  %1 = uitofp i8 %a to double
  ret double %1
}

define float @u32_to_float(i32 %a) {
; CHECK-LABEL: @u32_to_float
; CHECK: uitofp i32 {{.*}} to float
  %1 = uitofp i32 %a to float
  ret float %1
}

define double @u32_to_double(i32 %a) {
; CHECK-LABEL: @u32_to_double
; CHECK: uitofp i32 {{.*}} to double
  %1 = uitofp i32 %a to double
  ret double %1
}

define float @u51_to_float(i51 %a) {
; CHECK-LABEL: @u51_to_float
; CHECK: and i64 {{.*}}, 2251799813685247
; CHECK: uitofp i64 {{.*}} to float
  %1 = uitofp i51 %a to float
  ret float %1
}

define double @u51_to_double(i51 %a) {
; CHECK-LABEL: @u51_to_double
; CHECK: and i64 {{.*}}, 2251799813685247
; CHECK: uitofp i64 {{.*}} to double
  %1 = uitofp i51 %a to double
  ret double %1
}


define float @u64_to_float(i64 %a) {
; CHECK-LABEL: @u64_to_float
; CHECK: uitofp i64 {{.*}} to float
  %1 = uitofp i64 %a to float
  ret float %1
}

define double @u64_to_double(i64 %a) {
; CHECK-LABEL: @u64_to_double
; CHECK: uitofp i64 {{.*}} to double
  %1 = uitofp i64 %a to double
  ret double %1
}
