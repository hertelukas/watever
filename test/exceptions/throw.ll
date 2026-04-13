; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-wasip2"

@_ZTIi = external constant ptr

; Function Attrs: cold mustprogress norecurse noreturn
define hidden noundef i32 @__main_argc_argv(i32 noundef %0, ptr noundef readnone captures(none) %1) local_unnamed_addr #0 {
; CHECK-LABEL: $__main_argc_argv
; CHECK: i32.const 4

; calling __cxa_allocate_exception(4)
; CHECK-NEXT: call
; CHECK-NEXT: local.tee [[EXC_PTR:[0-9]+]]
; CHECK-NEXT: i32.const 1
; CHECK-NEXT: i32.store
; CHECK-NEXT: local.get [[EXC_PTR]]
; CHECK-NEXT: i32.const 0
; CHECK-NEXT: i32.const 0

; calling __cxa_throw
; CHECK-NEXT: call
  %3 = tail call ptr @__cxa_allocate_exception(i32 4) #2
  store i32 1, ptr %3, align 16
  tail call void @__cxa_throw(ptr nonnull %3, ptr nonnull @_ZTIi, ptr null) #3
  unreachable
}

declare ptr @__cxa_allocate_exception(i32) local_unnamed_addr

; Function Attrs: cold noreturn
declare void @__cxa_throw(ptr, ptr, ptr) local_unnamed_addr #1