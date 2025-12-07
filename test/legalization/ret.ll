; RUN: watever -l 0 --legal %s | FileCheck %s

define void @ret_void() {
; CHECK-LABEL: @ret_void
; CHECK: ret void
  ret void
}

define i28 @ret_i28(i28 %a) {
; CHECK-LABEL: @ret_i28
; CHECK: ret i32 %a
  ret i28 %a
}

define i32 @ret_i32(i32 %a) {
; CHECK-LABEL: @ret_i32
; CHECK: ret i32 %a
  ret i32 %a
}

define i33 @ret_i33(i33 %a) {
; CHECK-LABEL: @ret_i33
; CHECK: ret i64 %a
  ret i33 %a
}

define i64 @ret_i64(i64 %a) {
; CHECK-LABEL: @ret_i64
; CHECK: ret i64 %a
  ret i64 %a
}

define i128 @ret_i128_const() {
; CHECK-LABEL: void @ret_i128_const(ptr
; CHECK: store i64 10
; CHECK: store i64 0
  ret i128 10
}

define i128 @ret_i128(i128 %a) {
; CHECK-LABEL: void @ret_i128(ptr
; CHECK: store i64
; CHECK: store i64
  ret i128 %a
}

define i160 @ret_i160_const() {
; CHECK-LABEL: void @ret_i160_const(ptr
; CHECK: store i64 10
; CHECK: store i64 0
; CHECK: store i64 0
  ret i160 10
}
