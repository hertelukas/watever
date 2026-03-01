; RUN: watever -l 5 %s -o /dev/zero | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-unknown"

; As promoted stack slots are not in SSA, they have to live through
; predecessors of the block they have been defined in. This has lead to
; infinte loops - propagation must stop when visiting the loop header a second
; time.
; See test liveness.ll, promoted_alloca_starting_in_header for more information.
define i32 @foo() {
; CHECK-LABEL: Coloring function foo
entry:
  %ptr = alloca i32
  br label %loop

loop:
  store i32 0, ptr %ptr
  br label %loop
}