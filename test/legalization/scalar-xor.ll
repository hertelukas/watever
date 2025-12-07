; RUN: watever -l 0 --legal %s | FileCheck %s

define void @xor_i1_i1(i1 %a, i1 %b) {
; CHECK-LABEL: @xor_i1_i1
; CHECK: xor i32
entry:
  %0 = xor i1 %a, %b
  ret void
}

define void @xor_i8_1(i8 %a) {
; CHECK-LABEL: @xor_i8_1
; CHECK: xor i32 %a, 1 
entry:
  %1 = xor i8 %a, 1
  ret void
}

define void @xor_i8_i8(i8 %a, i8 %b) {
; CHECK-LABEL: @xor_i8_i8
; CHECK: xor i32
entry:
  %0 = xor i8 %a, %b
  ret void
}

define void @xor_i16_i16(i16 %a, i16 %b) {
; CHECK-LABEL: @xor_i16_i16
; CHECK: xor i32
entry:
  %0 = xor i16 %a, %b
  ret void
}

define void @xor_i32_i32(i32 %a, i32 %b) {
; CHECK-LABEL: @xor_i32_i32
; CHECK: xor i32
entry:
  %0 = xor i32 %a, %b
  ret void
}

define void @xor_i51_i51(i51 %a, i51 %b) {
; CHECK-LABEL: @xor_i51_i51
; CHECK: xor i64
entry:
  %0 = xor i51 %a, %b
  ret void
}

define void @xor_i64_i64(i64 %a, i64 %b) {
; CHECK-LABEL: @xor_i64_i64
; CHECK: xor i64
entry:
  %0 = xor i64 %a, %b
  ret void
}

