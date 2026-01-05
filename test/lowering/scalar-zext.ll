; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

; TODO test with --enable-sign-extension

define i32 @zext_i8_i32(i8 %a) {
; CHECK-LABEL: $zext_i8_i32
; CHECK:        local.get 0
; CHECK-NEXT:   i32.const 255
; CHECK-NEXT:   i32.and
; CHECK-NOT:    extend
    %1 = zext i8 %a to i32
    ret i32 %1
}

define i64 @zext_i8_i64(i8 %a) {
; CHECK-LABEL: $zext_i8_i64
; CHECK:        local.get 0
; CHECK-NEXT:   i32.const 255
; CHECK-NEXT:   i32.and
; CHECK-NEXT:   extend
    %1 = zext i8 %a to i64
    ret i64 %1
}

define i64 @zext_i32_i64(i32 %a) {
; CHECK-LABEL: $zext_i32_i64
; CHECK:        local.get 0
; CHECK-NEXT:   i64.extend_i32_u
; CHECK-NOT:    and
    %1 = zext i32 %a to i64
    ret i64 %1
}

define i64 @zext_i44_i64(i44 %a) {
; CHECK-LABEL: $zext_i44_i64
; CHECK:        local.get 0
; CHECK-NEXT:   i64.const 17592186044415
; CHECK-NEXT:   i64.and
; CHECK-NOT:    extend
    %1 = zext i44 %a to i64
    ret i64 %1
}