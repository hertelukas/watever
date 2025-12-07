; RUN: watever -l 0 --legal %s | FileCheck %s

define void @or_i1_i1(i1 %a, i1 %b) {
; CHECK-LABEL: @or_i1_i1
; CHECK: or i32
entry:
  %0 = or i1 %a, %b
  ret void
}

define void @or_i8_1(i8 %a) {
; CHECK-LABEL: @or_i8_1
; CHECK: or i32 %a, 1 
entry:
  %1 = or i8 %a, 1
  ret void
}

define void @or_i8_i8(i8 %a, i8 %b) {
; CHECK-LABEL: @or_i8_i8
; CHECK: or i32
entry:
  %0 = or i8 %a, %b
  ret void
}

define void @or_i16_i16(i16 %a, i16 %b) {
; CHECK-LABEL: @or_i16_i16
; CHECK: or i32
entry:
  %0 = or i16 %a, %b
  ret void
}

define void @or_i32_i32(i32 %a, i32 %b) {
; CHECK-LABEL: @or_i32_i32
; CHECK: or i32
entry:
  %0 = or i32 %a, %b
  ret void
}

define void @or_i51_i51(i51 %a, i51 %b) {
; CHECK-LABEL: @or_i51_i51
; CHECK: or i64
entry:
  %0 = or i51 %a, %b
  ret void
}

define void @or_i64_i64(i64 %a, i64 %b) {
; CHECK-LABEL: @or_i64_i64
; CHECK: or i64
entry:
  %0 = or i64 %a, %b
  ret void
}

