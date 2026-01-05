; RUN: watever -l 0 --legal %s | FileCheck %s

define void @void_target() {
  ret void
}

define void @illegal_arg_target(i128 %a) {
  ret void
}

define i128 @illegal_ret_target() {
  ret i128 100
}

define i128 @illegal_target(i128 %a) {
  ret i128 %a
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

define void @call_illegal_ret() {
; CHECK-LABEL: @call_illegal_ret
; CHECK: [[VAL1:%[0-9]+]] = alloca i128
; CHECK: call void @illegal_ret_target(ptr [[VAL1]])
  %1 = call i128 @illegal_ret_target()
  ret void
}

define void @call_illegal() {
; CHECK-LABEL: @call_illegal
; CHECK: [[VAL1:%[0-9]+]] = alloca i128
; CHECK: call void @illegal_target(ptr [[VAL1]], i64 100, i64 0)
  %1 = call i128 @illegal_target(i128 100)
  ret void
}