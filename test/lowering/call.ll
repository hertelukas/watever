; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

define void @void_target() {
  ret void
}

define void @illegal_arg_target(i128 %a) {
  ret void
}

define i32 @return_target() {
  ret i32 0
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