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

define void @arg_i128(i128 %a) {
; CHECK-LABEL: @arg_i128(i64 %a.0, i64 %a.1)
  ret void
}

define void @arg_i128_i64_i160(i128 %a, i64 %b, i160 %c) {
; CHECK-LABEL: @arg_i128_i64_i160(i64 %a.0, i64 %a.1, i64 %b, i64 %c.0, i64 %c.1, i64 %c.2)
  ret void
}

define void @vararg(i32 %a, ...) {
; CHECK-LABEL: @vararg(i32 %a, ptr %varargs)
  ret void
}