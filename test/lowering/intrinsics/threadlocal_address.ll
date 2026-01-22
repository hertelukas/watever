; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-unknown"

@tls_var = thread_local global i32 42

declare ptr @llvm.threadlocal.address.p0(ptr)

define i32 @read_tls() {
; CHECK-LABEL: $read_tls
; CHECK: i32.const 0
; CHECK-NEXT: i32.load
entry:
  %addr = call ptr @llvm.threadlocal.address.p0(ptr @tls_var)
  %val = load i32, ptr %addr
  ret i32 %val
}