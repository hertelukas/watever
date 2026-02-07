; RUN: watever %s -o %t.o
; RUN: wasm2wat %t.o | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-unknown"

define void @foo(ptr %p) {
; CHECK-LABEL: $foo
; CHECK: load
; The loaded pointer has to be set in a local, as it has multiple
; users in WebAssembly, even though it does not in LLVM
; CHECK-NEXT: local.tee
entry:
  %a = load ptr, ptr %p

; This is an offset and inlined twice - so %a has two users
; and needs a local.tee (for the first and second store), even though
; it has technically only one user
  %aint = ptrtoint ptr %a to i32
  %add = add i32 %aint, 42
  %b = inttoptr i32 %add to ptr
  store i32 1, ptr %b
  store i32 2, ptr %b
  ret void
}
