; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-unknown"

define i32 @select_i32(i1 %cond) {
; CHECK-LABEL: select_i32
; CHECK-NEXT:    i32.const 17
; CHECK-NEXT:    i32.const 42
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    i32.const 1
; CHECK-NEXT:    i32.and
; CHECK-NEXT:    select
entry:
  %1 = select i1 %cond, i32 17, i32 42
  ret i32 %1
}


;; TODO this should be tested more precisely
define i128 @select_i128(i1 %cond, i128 %a, i128 %b) {
; CHECK-LABEL: select_i128
; CHECK: and
; CHECK: select
; CHECK-NOT: and
; CHECK: select
entry:
  %1 = select i1 %cond, i128 %a, i128 %b
  ret i128 %1
}