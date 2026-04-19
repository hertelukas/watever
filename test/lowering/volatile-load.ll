; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-unknown"

define i8 @load_volatile_i8(ptr %ptr) {
; CHECK-LABEL: $load_volatile_i8
; CHECK-NEXT: local.get 0
; CHECK-NEXT: i32.load8_u
entry:
  %res = load volatile i8, ptr %ptr
  ret i8 %res
}
