; RUN: watever -l 0 --legal %s | FileCheck %s

define void @udiv_i1_i1(i1 %a, i1 %b) {
; CHECK-LABEL: @udiv_i1_i1
; CHECK:  and  i32  %a,  1
; CHECK:  and  i32  %b,  1
; CHECK:  udiv  i32  %0,  %1
entry:
  %0 = udiv i1 %a, %b
  ret void
}

define void @udiv_i8_3(i8 %a) {
; CHECK-LABEL: @udiv_i8_3
; CHECK: and i32 %a, 255
; CHECK: udiv i32 %0, 3
entry:
  %1 = udiv i8 %a, 3
  ret void
}

define void @udiv_i8_i8(i8 %a, i8 %b) {
; CHECK-LABEL: @udiv_i8_i8
; CHECK: and i32 %a, 255
; CHECK: and i32 %b, 255
; CHECK: udiv i32 %0, %1
entry:
  %0 = udiv i8 %a, %b
  ret void
}

define void @udiv_i16_i16(i16 %a, i16 %b) {
; CHECK-LABEL: @udiv_i16_i16
; CHECK: and i32 %a, 65535
; CHECK: and i32 %b, 65535
; CHECK: udiv i32 %0, %1
entry:
  %0 = udiv i16 %a, %b
  ret void
}

define void @udiv_i32_i32(i32 %a, i32 %b) {
; CHECK-LABEL: @udiv_i32_i32
; CHECK-NOT: and
; CHECK: udiv i32
entry:
  %0 = udiv i32 %a, %b
  ret void
}

define void @udiv_i51_i51(i51 %a, i51 %b) {
; CHECK-LABEL: @udiv_i51_i51
; CHECK: and i64 %a, 2251799813685247
; CHECK: and i64 %b, 2251799813685247
; CHECK: udiv i64
entry:
  %0 = udiv i51 %a, %b
  ret void
}

define void @udiv_i64_i64(i64 %a, i64 %b) {
; CHECK-LABEL: @udiv_i64_i64
; CHECK-NOT: and
; CHECK: udiv i64
entry:
  %0 = udiv i64 %a, %b
  ret void
}

