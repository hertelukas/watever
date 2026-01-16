; RUN: watever -l 0 %s --legal | FileCheck %s

define i8 @float_to_u8(float %a) {
; CHECK-LABEL: float_to_u8
; CHECK: fptoui float %a to i32
  %1 = fptoui float %a to i8 
  ret i8 %1
}

define i8 @float_to_i8(float %a) {
; CHECK-LABEL: float_to_i8
; CHECK: fptosi float %a to i32
  %1 = fptosi float %a to i8 
  ret i8 %1
}

define i32 @float_to_u32(float %a) {
; CHECK-LABEL: float_to_u32
; CHECK: fptoui float %a to i32
  %1 = fptoui float %a to i32 
  ret i32 %1
}

define i32 @float_to_i32(float %a) {
; CHECK-LABEL: float_to_i32
; CHECK: fptosi float %a to i32
  %1 = fptosi float %a to i32 
  ret i32 %1
}


define i44 @float_to_u44(float %a) {
; CHECK-LABEL: float_to_u44
; CHECK: fptoui float %a to i64
  %1 = fptoui float %a to i44 
  ret i44 %1
}

define i44 @float_to_i44(float %a) {
; CHECK-LABEL: float_to_i44
; CHECK: fptosi float %a to i64
  %1 = fptosi float %a to i44 
  ret i44 %1
}

define i8 @double_to_u8(double %a) {
; CHECK-LABEL: double_to_u8
; CHECK: fptoui double %a to i32
  %1 = fptoui double %a to i8 
  ret i8 %1
}

define i8 @double_to_i8(double %a) {
; CHECK-LABEL: double_to_i8
; CHECK: fptosi double %a to i32
  %1 = fptosi double %a to i8 
  ret i8 %1
}

define i32 @double_to_u32(double %a) {
; CHECK-LABEL: double_to_u32
; CHECK: fptoui double %a to i32
  %1 = fptoui double %a to i32 
  ret i32 %1
}

define i32 @double_to_i32(double %a) {
; CHECK-LABEL: double_to_i32
; CHECK: fptosi double %a to i32
  %1 = fptosi double %a to i32 
  ret i32 %1
}

define i44 @double_to_u44(double %a) {
; CHECK-LABEL: double_to_u44
; CHECK: fptoui double %a to i64
  %1 = fptoui double %a to i44 
  ret i44 %1
}

define i44 @double_to_i44(double %a) {
; CHECK-LABEL: double_to_i44
; CHECK: fptosi double %a to i64
  %1 = fptosi double %a to i44 
  ret i44 %1
}