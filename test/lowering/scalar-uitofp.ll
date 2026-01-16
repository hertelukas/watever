; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

define float @u8_to_float(i8 %a) {
; CHECK-LABEL: u8_to_float
; CHECK: local.get 0
; CHECK-NEXT: i32.const 255
; CHECK-NEXT: i32.and 
; CHECK-NEXT: f32.convert_i32_u
  %1 = uitofp i8 %a to float
  ret float %1
}

define double @u8_to_double(i8 %a) {
; CHECK-LABEL: u8_to_double
; CHECK: local.get 0
; CHECK-NEXT: i32.const 255
; CHECK-NEXT: i32.and 
; CHECK-NEXT: f64.convert_i32_u
  %1 = uitofp i8 %a to double
  ret double %1
}

define float @u32_to_float(i32 %a) {
; CHECK-LABEL: u32_to_float
; CHECK: local.get 0
; CHECK-NEXT: f32.convert_i32_u
  %1 = uitofp i32 %a to float
  ret float %1
}

define double @u32_to_double(i32 %a) {
; CHECK-LABEL: u32_to_double
; CHECK: local.get 0
; CHECK-NEXT: f64.convert_i32_u
  %1 = uitofp i32 %a to double
  ret double %1
}

define float @u51_to_float(i51 %a) {
; CHECK-LABEL: u51_to_float
; CHECK: local.get 0
; CHECK-NEXT: i64.const 2251799813685247
; CHECK-NEXT: i64.and 
; CHECK-NEXT: f32.convert_i64_u
  %1 = uitofp i51 %a to float
  ret float %1
}

define double @u51_to_double(i51 %a) {
; CHECK-LABEL: u51_to_double
; CHECK: local.get 0
; CHECK-NEXT: i64.const 2251799813685247
; CHECK-NEXT: i64.and 
; CHECK-NEXT: f64.convert_i64_u
  %1 = uitofp i51 %a to double
  ret double %1
}


define float @u64_to_float(i64 %a) {
; CHECK-LABEL: u64_to_float
; CHECK: local.get 0
; CHECK-NEXT: f32.convert_i64_u
  %1 = uitofp i64 %a to float
  ret float %1
}

define double @u64_to_double(i64 %a) {
; CHECK-LABEL: u64_to_double
; CHECK: local.get 0
; CHECK-NEXT: f64.convert_i64_u
  %1 = uitofp i64 %a to double
  ret double %1
}
