; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

define float @i8_to_float(i8 %a) {
; CHECK-LABEL: i8_to_float
; CHECK-NEXT: local.get 0
; CHECK-NEXT: i32.extend8_s
; CHECK-NEXT: f32.convert_i32_s
  %1 = sitofp i8 %a to float
  ret float %1
}

define double @i8_to_double(i8 %a) {
; CHECK-LABEL: i8_to_double
; CHECK-NEXT: local.get 0
; CHECK-NEXT: i32.extend8_s
; CHECK-NEXT: f64.convert_i32_s
  %1 = sitofp i8 %a to double
  ret double %1
}

define float @i32_to_float(i32 %a) {
; CHECK-LABEL: i32_to_float
; CHECK-NEXT: local.get 0
; CHECK-NEXT: f32.convert_i32_s
  %1 = sitofp i32 %a to float
  ret float %1
}

define double @i32_to_double(i32 %a) {
; CHECK-LABEL: i32_to_double
; CHECK-NEXT: local.get 0
; CHECK-NEXT: f64.convert_i32_s
  %1 = sitofp i32 %a to double
  ret double %1
}

define float @i51_to_float(i51 %a) {
; CHECK-LABEL: i51_to_float
; CHECK-NEXT: local.get 0
; CHECK-NEXT: i64.const 13
; CHECK-NEXT: i64.shl
; CHECK-NEXT: i64.const 13
; CHECK-NEXT: i64.shr_s
; CHECK-NEXT: f32.convert_i64_s
  %1 = sitofp i51 %a to float
  ret float %1
}

define double @i51_to_double(i51 %a) {
; CHECK-LABEL: i51_to_double
; CHECK-NEXT: local.get 0
; CHECK-NEXT: i64.const 13
; CHECK-NEXT: i64.shl
; CHECK-NEXT: i64.const 13
; CHECK-NEXT: i64.shr_s
; CHECK-NEXT: f64.convert_i64_s
  %1 = sitofp i51 %a to double
  ret double %1
}


define float @i64_to_float(i64 %a) {
; CHECK-LABEL: i64_to_float
; CHECK-NEXT: local.get 0
; CHECK-NEXT: f32.convert_i64_s
  %1 = sitofp i64 %a to float
  ret float %1
}

define double @i64_to_double(i64 %a) {
; CHECK-LABEL: i64_to_double
; CHECK-NEXT: local.get 0
; CHECK-NEXT: f64.convert_i64_s
  %1 = sitofp i64 %a to double
  ret double %1
}
