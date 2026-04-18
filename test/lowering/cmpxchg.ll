; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-unknown"

@x = hidden global i32 0, align 4

define hidden void @test() {
; CHECK-LABEL: $test
; CHECK:      i32.const       0
; CHECK-NEXT: i32.const       2
; CHECK-NEXT: i32.const       0
; CHECK-NEXT: i32.load
; CHECK-NEXT: local.tee       0
; CHECK-NEXT: local.get       0
; CHECK-NEXT: i32.const       1
; CHECK-NEXT: i32.eq
; CHECK-NEXT: select
; CHECK-NEXT: i32.store
  %1 = cmpxchg ptr @x, i32 1, i32 2 seq_cst seq_cst, align 4
  ret void
}
