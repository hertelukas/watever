; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

define i2 @srem_i2_i2(i2 %a, i2 %b) {
; CHECK-LABEL: (func $srem_i2_i2 {{.*}} (param i32 i32) (result i32)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    i32.const 30
; CHECK-NEXT:    i32.shl
; CHECK-NEXT:    i32.const 30
; CHECK-NEXT:    i32.shr_s
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i32.const 30
; CHECK-NEXT:    i32.shl
; CHECK-NEXT:    i32.const 30
; CHECK-NEXT:    i32.shr_s
; CHECK-NEXT:    i32.rem_s
entry:
  %0 = srem i2 %a, %b
  ret i2 %0
}

define i8 @srem_i8_3(i8 %a) {
; CHECK-LABEL:  (func $srem_i8_3 {{.*}} (param i32) (result i32)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    i32.const 24
; CHECK-NEXT:    i32.shl
; CHECK-NEXT:    i32.const 24
; CHECK-NEXT:    i32.shr_s
; CHECK-NEXT:    i32.const 3
; CHECK-NEXT:    i32.rem_s
entry:
  %1 = srem i8 %a, 3
  ret i8 %1
}


define i32 @srem_i32_i32(i32 %a, i32 %b) {
; CHECK-LABEL: (func $srem_i32_i32 {{.*}} (param i32 i32) (result i32)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i32.rem_s
entry:
  %0 = srem i32 %a, %b
  ret i32 %0
}

define i51 @srem_i51_i51(i51 %a, i51 %b) {
; CHECK-LABEL:  (func $srem_i51_i51 {{.*}} (param i64 i64) (result i64)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    i64.const 13
; CHECK-NEXT:    i64.shl
; CHECK-NEXT:    i64.const 13
; CHECK-NEXT:    i64.shr_s
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i64.const 13
; CHECK-NEXT:    i64.shl
; CHECK-NEXT:    i64.const 13
; CHECK-NEXT:    i64.shr_s
; CHECK-NEXT:    i64.rem_s
entry:
  %0 = srem i51 %a, %b
  ret i51 %0
}

define i64 @srem_i64_i64(i64 %a, i64 %b) {
; CHECK-LABEL: (func $srem_i64_i64 {{.*}} (param i64 i64) (result i64)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i64.rem_s
entry:
  %0 = srem i64 %a, %b
  ret i64 %0
}

