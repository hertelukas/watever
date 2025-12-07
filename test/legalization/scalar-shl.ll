; RUN: watever -l 0 --legal %s | FileCheck %s

define void @shl_i1_i1(i1 %a, i1 %b) {
; CHECK-LABEL: @shl_i1_i1
; CHECK: and i32 %b, 1
; CHECK: shl i32
entry:
  %0 = shl i1 %a, %b
  ret void
}

define void @shl_i8_1(i8 %a) {
; CHECK-LABEL: @shl_i8_1
; CHECK-NOT: and
; CHECK: shl i32 %a, 1 
entry:
  %1 = shl i8 %a, 1
  ret void
}

define void @shl_i8_i8(i8 %a, i8 %b) {
; CHECK-LABEL: @shl_i8_i8
; CHECK: and i32 %b, 255
; CHECK: shl i32
entry:
  %0 = shl i8 %a, %b
  ret void
}

define void @shl_i16_i16(i16 %a, i16 %b) {
; CHECK-LABEL: @shl_i16_i16
; CHECK: and i32 %b, 65535
; CHECK: shl i32
entry:
  %0 = shl i16 %a, %b
  ret void
}

define void @shl_i32_i32(i32 %a, i32 %b) {
; CHECK-LABEL: @shl_i32_i32
; CHECK-NOT: and
; CHECK: shl i32
entry:
  %0 = shl i32 %a, %b
  ret void
}

define void @shl_i51_i51(i51 %a, i51 %b) {
; CHECK-LABEL: @shl_i51_i51
; CHECK: and i64 %b, 2251799813685247
; CHECK: shl i64
entry:
  %0 = shl i51 %a, %b
  ret void
}

define void @shl_i64_i64(i64 %a, i64 %b) {
; CHECK-LABEL: @shl_i64_i64
; CHECK-NOT: and
; CHECK: shl i64
entry:
  %0 = shl i64 %a, %b
  ret void
}

