; RUN: watever -l 0 --legal %s | FileCheck %s

define void @mul_i1_i1(i1 %a, i1 %b) {
; CHECK-LABEL: @mul_i1_i1
; CHECK: mul i32
entry:
  %0 = mul i1 %a, %b
  ret void
}

define void @mul_i8_1(i8 %a) {
; CHECK-LABEL: @mul_i8_1
; CHECK: mul i32 %a, 1 
entry:
  %1 = mul nsw i8 %a, 1
  ret void
}

define void @mul_i8_i8(i8 %a, i8 %b) {
; CHECK-LABEL: @mul_i8_i8
; CHECK: mul i32
entry:
  %0 = mul i8 %a, %b
  ret void
}

define void @mul_i16_i16(i16 %a, i16 %b) {
; CHECK-LABEL: @mul_i16_i16
; CHECK: mul i32
entry:
  %0 = mul i16 %a, %b
  ret void
}

define void @mul_i32_i32(i32 %a, i32 %b) {
; CHECK-LABEL: @mul_i32_i32
; CHECK: mul i32
entry:
  %0 = mul i32 %a, %b
  ret void
}

define void @mul_i51_i51(i51 %a, i51 %b) {
; CHECK-LABEL: @mul_i51_i51
; CHECK: mul i64
entry:
  %0 = mul i51 %a, %b
  ret void
}

define void @mul_i64_i64(i64 %a, i64 %b) {
; CHECK-LABEL: @mul_i64_i64
; CHECK: mul i64
entry:
  %0 = mul i64 %a, %b
  ret void
}

