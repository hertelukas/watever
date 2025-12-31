; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

; TODO test with --enable-sign-extension

define i32 @sext_i8_i32(i8 %a) {
; CHECK-LABEL: sext_i8_i32
; CHECK-NEXT local.get 0
; CHECK-NEXT i32.const 24
; CHECK-NEXT i32.shl
; CHECK-NEXT i32.const 24
; CHECK-NEXT i32.shr_s
    %1 = sext i8 %a to i32
    ret i32 %1
}

define i64 @sext_i8_i64(i8 %a) {
; CHECK-LABEL: sext_i8_i64
; CHECK-NEXT local.get 0
; CHECK-NEXT i64.extend_i32_u
; CHECK-NEXT i64.const 56
; CHECK-NEXT i64.shl
; CHECK-NEXT i64.const 56
; CHECK-NEXT i64.shr_s
    %1 = sext i8 %a to i64
    ret i64 %1
}

define i64 @sext_i32_i64(i32 %a) {
; CHECK-LABEL: sext_i32_i64
; CHECK-NEXT:  local.get 0
; CHECK-NEXT:  i64.extend_i32_s
    %1 = sext i32 %a to i64
    ret i64 %1
}

define i64 @sext_i44_i64(i44 %a) {
; CHECK-LABEL: sext_i44_i64
; CHECK-NEXT local.get 0
; CHECK-NEXT i64.const 20
; CHECK-NEXT i64.shl
; CHECK-NEXT i64.const 20
; CHECK-NEXT i64.shr_s
    %1 = sext i44 %a to i64
    ret i64 %1
}