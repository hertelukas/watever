; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-unknown"

define void @void_target() {
  ret void
}

define void @illegal_arg_target(i128 %a) {
  ret void
}

define i32 @return_target() {
  ret i32 0
}

define i128 @illegal_ret_target() {
  ret i128 100
}

define i128 @illegal_target(i128 %a) {
  ret i128 %a
}

define void @call_void() {
; CHECK-LABEL: $call_void
; CHECK-NEXT: call $void_target
  call void @void_target()
  ret void
}

define void @call_illegal_arg() {
; CHECK-LABEL: $call_illegal_arg
; CHECK-NEXT:  i64.const 100
; CHECK-NEXT:  i64.const 0
; CHECK-NEXT:   call $illegal_arg_target
  call void @illegal_arg_target(i128 100)
  ret void
}

define i32 @call_used_result() {
; CHECK-LABEL: $call_used_result
; CHECK: call
; CHECK-NOT:  call
  %1 = call i32 @return_target()
  ret i32 %1
}

define i32 @call_order() {
; CHECK-LABEL: $call_order
; CHECK: call $return_target
; CHECK: local.set [[VAL1:[0-9]+]]
; CHECK: call $void_target
; CHECK: local.get [[VAL1]]
  %1 = call i32 @return_target()
  call void @void_target()
  ret i32 %1
}

define void @call_illegal_ret() {
; CHECK-LABEL: $call_illegal_ret
; CHECK: global.get
; CHECK-NEXT: i32.const 16
; CHECK-NEXT: i32.sub
; CHECK: global.set
; CECK: call $illegal_arg_target
  %1 = call i128 @illegal_ret_target()
  ret void
}

define void @call_illegal() {
; CHECK-LABEL: $call_illegal
; CHECK: global.get
; CHECK-NEXT: i32.const 16
; CHECK-NEXT: i32.sub
; CHECK: global.set
; CHECK: i64.const 100
; CHECK-NEXT: i64.const 0
; CHECK-NEXT: call $illegal_target
  %1 = call i128 @illegal_target(i128 100)
  ret void
}