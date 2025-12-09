; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

define void @void_target() {
  ret void
}

define void @illegal_arg_target(i128 %a) {
  ret void
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
