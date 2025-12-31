; RUN: watever -l 0 --legal %s | FileCheck %s

; TODO test with --enable-sign-extension

define i32 @sext_i8_i32(i8 %a) {
; CHECK-LABEL: @sext_i8_i32
; CHECK-NEXT:   %1 = shl i32 %a, 24
; CHECK-NEXT:   %2 = ashr i32 %1, 24
; CHECK-NEXT:   ret i32 %2
    %1 = sext i8 %a to i32
    ret i32 %1
}

define i64 @sext_i8_i64(i8 %a) {
; CHECK-LABEL: @sext_i8_i64
; CHECK-NEXT:   %1 = zext i32 %a to i64
; CHECK-NEXT:   %2 = shl i64 %1, 56
; CHECK-NEXT:   %3 = ashr i64 %2, 56
; CHECK-NEXT:   ret i64 %3
    %1 = sext i8 %a to i64
    ret i64 %1
}

define i64 @sext_i32_i64(i32 %a) {
; CHECK-LABEL: @sext_i32_i64
; CHECK-NEXT:   %1 = sext i32 %a to i64
; CHECK-NEXT:   ret i64 %1
    %1 = sext i32 %a to i64
    ret i64 %1
}

define i64 @sext_i44_i64(i44 %a) {
; CHECK-LABEL: @sext_i44_i64
; CHECK-NEXT: %1 = shl i64 %a, 20
; CHECK-NEXT: %2 = ashr i64 %1, 20
    %1 = sext i44 %a to i64
    ret i64 %1
}