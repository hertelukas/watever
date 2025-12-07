; RUN: watever -l 0 --legal %s | FileCheck %s

define void @lshr_i1_i1(i1 %a, i1 %b) {
; CHECK-LABEL: @lshr_i1_i1
; CHECK: and i32 %a, 1
; CHECK: and i32 %b, 1
; CHECK: lshr i32
entry:
  %0 = lshr i1 %a, %b
  ret void
}

define void @lshr_i8_1(i8 %a) {
; CHECK-LABEL: @lshr_i8_1
; CHECK: and i32 %a, {{254|255}}
; CHECK: lshr i32 {{.*}}, 1 
entry:
  %1 = lshr i8 %a, 1
  ret void
}

define void @lshr_i8_i8(i8 %a, i8 %b) {
; CHECK-LABEL: @lshr_i8_i8
; CHECK: and i32 %a, 255
; CHECK: and i32 %b, 255
; CHECK: lshr i32
entry:
  %0 = lshr i8 %a, %b
  ret void
}

define void @lshr_i16_i16(i16 %a, i16 %b) {
; CHECK-LABEL: @lshr_i16_i16
; CHECK: and i32 %a, 65535
; CHECK: and i32 %b, 65535
; CHECK: lshr i32
entry:
  %0 = lshr i16 %a, %b
  ret void
}

define void @lshr_i32_i32(i32 %a, i32 %b) {
; CHECK-LABEL: @lshr_i32_i32
; CHECK-NOT: and
; CHECK: lshr i32
entry:
  %0 = lshr i32 %a, %b
  ret void
}

define void @lshr_i51_i51(i51 %a, i51 %b) {
; CHECK-LABEL: @lshr_i51_i51
; CHECK: and i64 %a, 2251799813685247
; CHECK: and i64 %b, 2251799813685247
; CHECK: lshr i64
entry:
  %0 = lshr i51 %a, %b
  ret void
}

define void @lshr_i64_i64(i64 %a, i64 %b) {
; CHECK-LABEL: @lshr_i64_i64
; CHECK-NOT: and
; CHECK: lshr i64
entry:
  %0 = lshr i64 %a, %b
  ret void
}

