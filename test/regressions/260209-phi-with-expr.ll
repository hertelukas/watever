; RUN: watever %s --legal_opt | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-unknown"

@.str = private unnamed_addr constant [5 x i8] c"null\00", align 1

define ptr @foo(i1 %cond) {
; CHECK-LABEL: define ptr @foo
; CHECK: if:
; CHECK-NEXT: ptrtoint ptr @.str to i32
; CHECK-NEXT: add i32 {{.*}}, 3
; CHECK-NEXT: [[PTR:%[0-9]+]] = inttoptr i32
; CHECK: merge:
; CHECK-NEXT: [ [[PTR]], %if ]
entry: 
  %ptr = alloca i8
  br i1 %cond, label %if, label %merge
if:
  br label %merge
merge:
  %res = phi ptr [ %ptr, %entry ], [ getelementptr (i8, ptr @.str, i32 3), %if ]
  ret ptr %res
}
