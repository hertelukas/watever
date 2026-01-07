; RUN: watever -l 0 --legal %s | FileCheck %s

; TODO test without sign-extension

define i32 @sext_i7_i32(i7 %a) {
; CHECK-LABEL: @sext_i7_i32
; CHECK-NEXT:   %1 = shl i32 %a, 25
; CHECK-NEXT:   %2 = ashr i32 %1, 25
; CHECK-NEXT:   ret i32 %2
    %1 = sext i7 %a to i32
    ret i32 %1
}

define i32 @sext_i8_i32(i8 %a) {
; CHECK-LABEL: @sext_i8_i32
; CHECK: sext i8 {{.*}} to i32
; CHECK-NOT:  shl
    %1 = sext i8 %a to i32
    ret i32 %1
}

define i32 @sext_i15_i32(i15 %a) {
; CHECK-LABEL: @sext_i15_i32
; CHECK-NEXT:   %1 = shl i32 %a, 17
; CHECK-NEXT:   %2 = ashr i32 %1, 17
; CHECK-NEXT:   ret i32 %2
    %1 = sext i15 %a to i32
    ret i32 %1
}

define i32 @sext_i16_i32(i16 %a) {
; CHECK-LABEL: @sext_i16_i32
; CHECK:  sext i16 {{.*}} to i32
; CHECK-NOT: shl
    %1 = sext i16 %a to i32
    ret i32 %1
}

define i64 @sext_i8_i64(i8 %a) {
; CHECK-LABEL: @sext_i8_i64
; CHECK:  sext i8 {{.*}} to i64
; CHECK-NOT: shl
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