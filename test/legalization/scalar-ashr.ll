; RUN: watever -l 0 --legal %s | FileCheck %s

define void @ashr_i1_i1(i1 %a, i1 %b) {
; CHECK-LABEL: @ashr_i1_i1
; CHECK: shl i32 %a, 31
; CHECK: ashr i32 %0, 31
; CHECK: and i32 %b, 1
; CHECK: ashr i32 %1, %2
entry:
  %0 = ashr i1 %a, %b
  ret void
}

define void @ashr_i8_1(i8 %a) {
; CHECK-LABEL: @ashr_i8_1
; CHECK: trunc i32 %a to i8
; CHECK-NEXT: sext i8 {{.*}} to i32
; CHECK: ashr i32 %1, 1
entry:
  %1 = ashr i8 %a, 1
  ret void
}

define void @ashr_i8_i8(i8 %a, i8 %b) {
; CHECK-LABEL: @ashr_i8_i8
; CHECK: trunc i32 %a to i8
; CHECK-NEXT: sext i8 {{.*}} to i32
; CHECK: and i32 %b, 255
; CHECK: ashr i32 %1, %2
entry:
  %0 = ashr i8 %a, %b
  ret void
}

define void @ashr_i16_i16(i16 %a, i16 %b) {
; CHECK-LABEL: @ashr_i16_i16
; CHECK: trunc i32 {{.*}} to i16
; CHECK-NEXT: sext i16 {{.*}} to i32
; CHECK: and i32 %b, 65535
; CHECK: ashr i32 %1, %2
entry:
  %0 = ashr i16 %a, %b
  ret void
}

define void @ashr_i32_i32(i32 %a, i32 %b) {
; CHECK-LABEL: @ashr_i32_i32
; CHECK-NOT: shl
; CHECK: ashr i32
entry:
  %0 = ashr i32 %a, %b
  ret void
}

define void @ashr_i51_i51(i51 %a, i51 %b) {
; CHECK-LABEL: @ashr_i51_i51
; CHECK: shl i64 %a, 13
; CHECK: ashr i64 %0, 13
; CHECK: and i64 %b, 2251799813685247
; CHECK: ashr i64 %1, %2
entry:
  %0 = ashr i51 %a, %b
  ret void
}

define void @ashr_i64_i64(i64 %a, i64 %b) {
; CHECK-LABEL: @ashr_i64_i64
; CHECK-NOT: shl
; CHECK: ashr i64
entry:
  %0 = ashr i64 %a, %b
  ret void
}

