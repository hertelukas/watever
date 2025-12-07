; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

define i2 @lshr_i2_i2(i2 %a, i2 %b) {
; CHECK-LABEL: (func $lshr_i2_i2 {{.*}} (param i32 i32) (result i32)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    i32.const 3
; CHECK-NEXT:    i32.and
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i32.const 3
; CHECK-NEXT:    i32.and
; CHECK-NEXT:    i32.shr_u)
entry:
  %0 = lshr i2 %a, %b
  ret i2 %0
}

define i8 @lshr_i8_3(i8 %a) {
; CHECK-LABEL:  (func $lshr_i8_3 {{.*}} (param i32) (result i32)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    i32.const {{24[8-9]|25[0-5]}}
; CHECK-NEXT:    i32.and
; CHECK-NEXT:    i32.const 3
; CHECK-NEXT:    i32.shr_u)
entry:
  %1 = lshr i8 %a, 3
  ret i8 %1
}


define i32 @lshr_i32_i32(i32 %a, i32 %b) {
; CHECK-LABEL: (func $lshr_i32_i32 {{.*}} (param i32 i32) (result i32)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i32.shr_u)
entry:
  %0 = lshr i32 %a, %b
  ret i32 %0
}

define i51 @lshr_i51_i51(i51 %a, i51 %b) {
; CHECK-LABEL:  (func $lshr_i51_i51 {{.*}} (param i64 i64) (result i64)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    i64.const 2251799813685247
; CHECK-NEXT:    i64.and
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i64.const 2251799813685247
; CHECK-NEXT:    i64.and
; CHECK-NEXT:    i64.shr_u)
entry:
  %0 = lshr i51 %a, %b
  ret i51 %0
}

define i64 @lshr_i64_i64(i64 %a, i64 %b) {
; CHECK-LABEL: (func $lshr_i64_i64 {{.*}} (param i64 i64) (result i64)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i64.shr_u)
entry:
  %0 = lshr i64 %a, %b
  ret i64 %0
}

