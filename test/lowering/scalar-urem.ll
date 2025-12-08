; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

define i2 @urem_i2_i2(i2 %a, i2 %b) {
; CHECK-LABEL: (func $urem_i2_i2 {{.*}} (param i32 i32) (result i32)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    i32.const 3
; CHECK-NEXT:    i32.and
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i32.const 3
; CHECK-NEXT:    i32.and
; CHECK-NEXT:    i32.rem_u
entry:
  %0 = urem i2 %a, %b
  ret i2 %0
}

define i8 @urem_i8_3(i8 %a) {
; CHECK-LABEL:  (func $urem_i8_3 {{.*}} (param i32) (result i32)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    i32.const 255
; CHECK-NEXT:    i32.and
; CHECK-NEXT:    i32.const 3
; CHECK-NEXT:    i32.rem_u
entry:
  %1 = urem i8 %a, 3
  ret i8 %1
}


define i32 @urem_i32_i32(i32 %a, i32 %b) {
; CHECK-LABEL: (func $urem_i32_i32 {{.*}} (param i32 i32) (result i32)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i32.rem_u
entry:
  %0 = urem i32 %a, %b
  ret i32 %0
}

define i51 @urem_i51_i51(i51 %a, i51 %b) {
; CHECK-LABEL:  (func $urem_i51_i51 {{.*}} (param i64 i64) (result i64)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    i64.const 2251799813685247
; CHECK-NEXT:    i64.and
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i64.const 2251799813685247
; CHECK-NEXT:    i64.and
; CHECK-NEXT:    i64.rem_u
entry:
  %0 = urem i51 %a, %b
  ret i51 %0
}

define i64 @urem_i64_i64(i64 %a, i64 %b) {
; CHECK-LABEL: (func $urem_i64_i64 {{.*}} (param i64 i64) (result i64)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i64.rem_u
entry:
  %0 = urem i64 %a, %b
  ret i64 %0
}

