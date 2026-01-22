; RUN: watever %s -o %t.o
; RUN: wasm-objdump -x %t.o | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-unknown"

declare void @import_func()

define internal void @local_func() {
  ret void
}

define weak void @weak_func() {
  ret void
}

define linkonce void @linkonce_func() {
  ret void
}

define linkonce_odr void @linkonce_odr_func() {
  ret void
}

define hidden void @hidden_func() {
  ret void
}

@tls_global = thread_local global i32 42, align 4

define void @main() {
  call void @import_func()
  call void @local_func()
  call void @weak_func()
  call void @linkonce_func()
  call void @linkonce_odr_func()
  call void @hidden_func()
  %1 = load i32, ptr @tls_global
  ret void
}

; CHECK: name: "linking"
; CHECK: <tls_global> {{.*}} [ binding=global vis=default ]
; CHECK: <env.import_func> {{.*}} [ undefined binding=global vis=default ]
; CHECK: <local_func> {{.*}} [ binding=local vis=default ]
; CHECK: <weak_func> {{.*}} [ binding=weak vis=default ]
; CHECK: <linkonce_func> {{.*}} [ binding=weak vis=default ]
; CHECK: <linkonce_odr_func> {{.*}} [ binding=weak vis=default ]
; CHECK: <hidden_func> {{.*}} [ binding=global vis=hidden ]
