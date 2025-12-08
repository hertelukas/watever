; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

define i2 @sub_i2_i2(i2 %a, i2 %b) {
; CHECK-LABEL: (func $sub_i2_i2 {{.*}} (param i32 i32) (result i32)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i32.sub
entry:
  %0 = sub i2 %a, %b
  ret i2 %0
}

define i8 @sub_i8_1(i8 %a) {
; CHECK-LABEL:  (func $sub_i8_1 {{.*}} (param i32) (result i32)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    i32.const 1
; CHECK-NEXT:    i32.sub
entry:
  %1 = sub nsw i8 %a, 1
  ret i8 %1
}


define i32 @sub_i32_i32(i32 %a, i32 %b) {
; CHECK-LABEL: (func $sub_i32_i32 {{.*}} (param i32 i32) (result i32)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i32.sub
entry:
  %0 = sub i32 %a, %b
  ret i32 %0
}

define i51 @sub_i51_i51(i51 %a, i51 %b) {
; CHECK-LABEL:  (func $sub_i51_i51 {{.*}} (param i64 i64) (result i64)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i64.sub
entry:
  %0 = sub i51 %a, %b
  ret i51 %0
}

define i64 @sub_i64_i64(i64 %a, i64 %b) {
; CHECK-LABEL: (func $sub_i64_i64 {{.*}} (param i64 i64) (result i64)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i64.sub
entry:
  %0 = sub i64 %a, %b
  ret i64 %0
}

