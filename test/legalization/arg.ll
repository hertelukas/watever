; RUN: watever -l 0 --legal %s | FileCheck %s

define void @arg_void() {
; CHECK-LABEL: @arg_void()
  ret void
}

define void @arg_i28(i28 %a) {
; CHECK-LABEL: @arg_i28(i32 %a)
  ret void
}

define void @arg_i32(i32 %a) {
; CHECK-LABEL: @arg_i32(i32 %a)
  ret void
}

define void @arg_i33(i33 %a) {
; CHECK-LABEL: @arg_i33(i64 %a)
  ret void
}

define void @arg_i64(i64 %a) {
; CHECK-LABEL: @arg_i64(i64 %a)
  ret void
}
