; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

define i32 @add_i32_i32(i32 %a, i32 %b) {
; CHECK-LABEL: (func $add_i32_i32 {{.*}} (param i32 i32) (result i32)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i32.add)
entry:
  %0 = add i32 %a, %b
  ret i32 %0
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

