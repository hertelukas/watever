; RUN: watever -l 0 --legal %s | FileCheck %s

define void @void_target() {
  ret void
}

define void @illegal_arg_target(i128 %a) {
  ret void
}

define void @call_void() {
; CHECK-LABEL: @call_void
; CHECK-NEXT: call void @void_target
  call void @void_target()
  ret void
}

define void @call_illegal_arg() {
; CHECK-LABEL: @call_illegal_arg
; CHECK-NEXT:   call void @illegal_arg_target(i64 100, i64 0)
  call void @illegal_arg_target(i128 100)
  ret void
}
