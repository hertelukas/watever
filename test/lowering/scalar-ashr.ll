; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

define i2 @ashr_i2_i2(i2 %a, i2 %b) {
; CHECK-LABEL: (func $ashr_i2_i2 {{.*}} (param i32 i32) (result i32)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    i32.const 30
; CHECK-NEXT:    i32.shl
; CHECK-NEXT:    i32.const 30
; CHECK-NEXT:    i32.shr_s
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i32.const 3
; CHECK-NEXT:    i32.and
; CHECK-NEXT:    i32.shr_s
entry:
  %0 = ashr i2 %a, %b
  ret i2 %0
}

define i8 @ashr_i8_1(i8 %a) {
; CHECK-LABEL:  (func $ashr_i8_1 {{.*}} (param i32) (result i32)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    i32.extend8_s
; CHECK-NEXT:    i32.const 1
; CHECK-NEXT:    i32.shr_s
entry:
  %1 = ashr i8 %a, 1
  ret i8 %1
}

define i32 @ashr_i32_i32(i32 %a, i32 %b) {
; CHECK-LABEL: (func $ashr_i32_i32 {{.*}} (param i32 i32) (result i32)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i32.shr_s
entry:
  %0 = ashr i32 %a, %b
  ret i32 %0
}

define i51 @ashr_i51_i51(i51 %a, i51 %b) {
; CHECK-LABEL:  (func $ashr_i51_i51 {{.*}} (param i64 i64) (result i64)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    i64.const 13
; CHECK-NEXT:    i64.shl
; CHECK-NEXT:    i64.const 13
; CHECK-NEXT:    i64.shr_s
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i64.const 2251799813685247
; CHECK-NEXT:    i64.and
; CHECK-NEXT:    i64.shr_s
entry:
  %0 = ashr i51 %a, %b
  ret i51 %0
}

define i64 @ashr_i64_i64(i64 %a, i64 %b) {
; CHECK-LABEL: (func $ashr_i64_i64 {{.*}} (param i64 i64) (result i64)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i64.shr_s
entry:
  %0 = ashr i64 %a, %b
  ret i64 %0
}

