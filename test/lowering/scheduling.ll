; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

define i32 @everything_live(i32 %a, i32 %b) {
; CHECK-LABEL: everything_live
; CHECK-NEXT:    (local 
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i32.add
; CHECK-NEXT:    local.tee [[VAL1:[0-9]+]]
; CHECK-NEXT:    i32.and
; CHECK-NEXT:    i32.const 0
; CHECK-NEXT:    local.get [[VAL1]]
; CHECK-NEXT:    i32.sub
; CHECK-NEXT:    local.tee [[VAL3:[0-9]+]]
; CHECK-NEXT:    i32.add
; CHECK-NEXT:    local.get [[VAL3]]
; CHECK-NEXT:    i32.mul
; CHECK-NEXT:    return
entry:
  %1 = add i32 %a, %b
  %2 = and i32 %a, %1
  %3 = sub i32 0, %1
  %4 = add i32 %2, %3
  %5 = mul i32 %4, %3
  ret i32 %5
}

;; The multiplication has to be materialized in the return,
;; even though earlier instructions need it
define i32 @dead_users(i32 %a, i32 %b) {
; CHECK-LABEL: dead_users
; CHECK-NEXT:    (local 
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i32.add
; CHECK-NEXT:    local.tee [[VAL1:[0-9]+]]
; CHECK-NEXT:    i32.and
; CHECK-NEXT:    i32.const 0
; CHECK-NEXT:    local.get [[VAL1]]
; CHECK-NEXT:    i32.sub
; CHECK-NEXT:    local.tee [[VAL3:[0-9]+]]
; CHECK-NEXT:    i32.add
; CHECK-NEXT:    local.get [[VAL3]]
; CHECK-NEXT:    i32.mul
; CHECK-NEXT:    return
entry:
  %1 = add i32 %a, %b
  %2 = and i32 %a, %1
  %3 = sub i32 0, %1
  %4 = add i32 %2, %3
  %5 = mul i32 %4, %3
  %6 = and i32 %5, 3
  ret i32 %5
}