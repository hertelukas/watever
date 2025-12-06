; RUN: watever -l 0 --legal %s | FileCheck %s

define void @sub_i1_i1(i1 %a, i1 %b) {
; CHECK-LABEL: @sub_i1_i1
; CHECK: sub i32
entry:
  %0 = sub i1 %a, %b
  ret void
}

define void @sub_i8_1(i8 %a) {
; CHECK-LABEL: @sub_i8_1
; CHECK: sub i32 %a, 1 
entry:
  %1 = sub nsw i8 %a, 1
  ret void
}

define void @sub_i8_i8(i8 %a, i8 %b) {
; CHECK-LABEL: @sub_i8_i8
; CHECK: sub i32
entry:
  %0 = sub i8 %a, %b
  ret void
}

define void @sub_i16_i16(i16 %a, i16 %b) {
; CHECK-LABEL: @sub_i16_i16
; CHECK: sub i32
entry:
  %0 = sub i16 %a, %b
  ret void
}

define void @sub_i32_i32(i32 %a, i32 %b) {
; CHECK-LABEL: @sub_i32_i32
; CHECK: sub i32
entry:
  %0 = sub i32 %a, %b
  ret void
}

define void @sub_i51_i51(i51 %a, i51 %b) {
; CHECK-LABEL: @sub_i51_i51
; CHECK: sub i64
entry:
  %0 = sub i51 %a, %b
  ret void
}

define void @sub_i64_i64(i64 %a, i64 %b) {
; CHECK-LABEL: @sub_i64_i64
; CHECK: sub i64
entry:
  %0 = sub i64 %a, %b
  ret void
}

