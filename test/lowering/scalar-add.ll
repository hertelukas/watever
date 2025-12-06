; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

define i2 @add_i2_i2(i2 %a, i2 %b) {
; CHECK-LABEL: (func $add_i2_i2 {{.*}} (param i32 i32) (result i32)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i32.add)
entry:
  %0 = add i2 %a, %b
  ret i2 %0
}

define i8 @add_i8_1(i8 %a) {
; CHECK-LABEL:  (func $add_i8_1 {{.*}} (param i32) (result i32)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    i32.const 1
; CHECK-NEXT:    i32.add)
entry:
  %1 = add nsw i8 %a, 1
  ret i8 %1
}


define i32 @add_i32_i32(i32 %a, i32 %b) {
; CHECK-LABEL: (func $add_i32_i32 {{.*}} (param i32 i32) (result i32)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i32.add)
entry:
  %0 = add i32 %a, %b
  ret i32 %0
}

define i51 @add_i51_i51(i51 %a, i51 %b) {
; CHECK-LABEL:  (func $add_i51_i51 {{.*}} (param i64 i64) (result i64)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i64.add)
entry:
  %0 = add i51 %a, %b
  ret i51 %0
}

define i64 @add_i64_i64(i64 %a, i64 %b) {
; CHECK-LABEL: (func $add_i64_i64 {{.*}} (param i64 i64) (result i64)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i64.add)
entry:
  %0 = add i64 %a, %b
  ret i64 %0
}

