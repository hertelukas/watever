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

