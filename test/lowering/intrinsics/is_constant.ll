; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-unknown"

@my_global = global i32 123, align 4

define i1 @is_constant_true() {
; CHECK-LABEL: $is_constant_true
; CHECK: i32.const 1
  %1 = call i1 @llvm.is.constant.i32(i32 42)
  ret i1 %1
}

define i1 @is_constant_false(i32 %a) {
; CHECK-LABEL: $is_constant_false
; CHECK: i32.const 0
  %1 = call i1 @llvm.is.constant.i32(i32 %a)
  ret i1 %1
}

define i1 @is_constant_expr() {
; CHECK-LABEL: $is_constant_expr
; CHECK: i32.const 1
  %val = add i32 10, 20
  %1 = call i1 @llvm.is.constant.i32(i32 %val)
  ret i1 %1
}

define i1 @is_constant_global() {
; CHECK-LABEL: $is_constant_global
; CHECK: i32.const 0
  %ptr = getelementptr i32, ptr @my_global, i32 1
  %1 = call i1 @llvm.is.constant.p0(ptr %ptr)
  ret i1 %1
}