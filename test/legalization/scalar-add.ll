; RUN: watever -l 0 --legal %s | FileCheck %s

define void @add_i1_i1(i1 %a, i1 %b) {
; CHECK-LABEL: @add_i1_i1
; CHECK: add i32
entry:
  %0 = add i1 %a, %b
  ret void
}

define void @add_i8_1(i8 %a) {
; CHECK-LABEL: @add_i8_1
; CHECK: add i32 {{%[0-9]+}}, 1 
entry:
  %1 = add nsw i8 %a, 1
  ret void
}

define void @add_i8_i8(i8 %a, i8 %b) {
; CHECK-LABEL: @add_i8_i8
; CHECK: add i32
entry:
  %0 = add i8 %a, %b
  ret void
}

define void @add_i16_i16(i16 %a, i16 %b) {
; CHECK-LABEL: @add_i16_i16
; CHECK: add i32
entry:
  %0 = add i16 %a, %b
  ret void
}

define void @add_i32_i32(i32 %a, i32 %b) {
; CHECK-LABEL: @add_i32_i32
; CHECK: add i32
entry:
  %0 = add i32 %a, %b
  ret void
}

define void @add_i64_i64(i64 %a, i64 %b) {
; CHECK-LABEL: @add_i64_i64
; CHECK: add i64
entry:
  %0 = add i64 %a, %b
  ret void
}

define void @add_i128_i128(i128 %a, i128 %b) {
; CHECK-LABEL: @add_i128_i128
; CHECK: add i64
; CHECK: icmp ult i64
; CHECK: add i64
; CHECK: add i64
entry:
  %0 = add i128 %a, %b
  ret void
}

define void @add_i128_imm(i128 %a) {
; CHECK-LABEL: @add_i128_imm
; CHECK: add i64 {{.*}}, 50
; CHECK: icmp ult i64
; CHECK: add i64
entry:
  %0 = add i128 %a, 50
  ret void
}