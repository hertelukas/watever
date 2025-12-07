; RUN: watever -l 0 --legal %s | FileCheck %s

define void @srem_i1_i1(i1 %a, i1 %b) {
; CHECK-LABEL: @srem_i1_i1
; CHECK: shl i32 %a, 31
; CHECK-NEXT: ashr i32 %0, 31
; CHECK-NEXT: shl i32 %b, 31
; CHECK-NEXT: ashr i32 %2, 31
; CHECK-NEXT: srem i32 %1, %3
entry:
  %0 = srem i1 %a, %b
  ret void
}

define void @srem_i8_3(i8 %a) {
; CHECK-LABEL: @srem_i8_3
; CHECK: shl i32 %a, 24
; CHECK-NEXT: ashr i32 %0, 24
; CHECK-NEXT: srem i32 %1, 3
entry:
  %1 = srem i8 %a, 3
  ret void
}

define void @srem_i8_i8(i8 %a, i8 %b) {
; CHECK-LABEL: @srem_i8_i8
; CHECK: shl i32 %a, 24
; CHECK-NEXT: ashr i32 %0, 24
; CHECK-NEXT: shl i32 %b, 24
; CHECK-NEXT: ashr i32 %2, 24
; CHECK-NEXT: srem i32 %1, %3
entry:
  %0 = srem i8 %a, %b
  ret void
}

define void @srem_i16_i16(i16 %a, i16 %b) {
; CHECK-LABEL: @srem_i16_i16
; CHECK: shl i32 %a, 16
; CHECK-NEXT: ashr i32 %0, 16
; CHECK-NEXT: shl i32 %b, 16
; CHECK-NEXT: ashr i32 %2, 16
; CHECK-NEXT: srem i32 %1, %3
entry:
  %0 = srem i16 %a, %b
  ret void
}

define void @srem_i32_i32(i32 %a, i32 %b) {
; CHECK-LABEL: @srem_i32_i32
; CHECK-NOT: ashr
; CHECK-NOT: shl
; CHECK: srem i32
entry:
  %0 = srem i32 %a, %b
  ret void
}

define void @srem_i51_i51(i51 %a, i51 %b) {
; CHECK-LABEL: @srem_i51_i51
; CHECK: shl i64 %a, 13
; CHECK: ashr i64 %0, 13
; CHECK: shl i64 %b, 13
; CHECK: ashr i64 %2, 13
; CHECK: srem i64 %1, %3
entry:
  %0 = srem i51 %a, %b
  ret void
}

define void @srem_i64_i64(i64 %a, i64 %b) {
; CHECK-LABEL: @srem_i64_i64
; CHECK-NOT: ashr
; CHECK-NOT: shl
; CHECK: srem i64
entry:
  %0 = srem i64 %a, %b
  ret void
}
