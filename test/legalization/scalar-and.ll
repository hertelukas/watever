; RUN: watever -l 0 --legal %s | FileCheck %s

define void @and_i1_i1(i1 %a, i1 %b) {
; CHECK-LABEL: @and_i1_i1
; CHECK: and i32
entry:
  %0 = and i1 %a, %b
  ret void
}

define void @and_i8_1(i8 %a) {
; CHECK-LABEL: @and_i8_1
; CHECK: and i32 %a, 1 
entry:
  %1 = and i8 %a, 1
  ret void
}

define void @and_i8_i8(i8 %a, i8 %b) {
; CHECK-LABEL: @and_i8_i8
; CHECK: and i32
entry:
  %0 = and i8 %a, %b
  ret void
}

define void @and_i16_i16(i16 %a, i16 %b) {
; CHECK-LABEL: @and_i16_i16
; CHECK: and i32
entry:
  %0 = and i16 %a, %b
  ret void
}

define void @and_i32_i32(i32 %a, i32 %b) {
; CHECK-LABEL: @and_i32_i32
; CHECK: and i32
entry:
  %0 = and i32 %a, %b
  ret void
}

define void @and_i51_i51(i51 %a, i51 %b) {
; CHECK-LABEL: @and_i51_i51
; CHECK: and i64
entry:
  %0 = and i51 %a, %b
  ret void
}

define void @and_i64_i64(i64 %a, i64 %b) {
; CHECK-LABEL: @and_i64_i64
; CHECK: and i64
entry:
  %0 = and i64 %a, %b
  ret void
}

