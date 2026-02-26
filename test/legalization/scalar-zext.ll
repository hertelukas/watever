; RUN: watever -l 0 --legal %s | FileCheck %s

define i32 @zext_i8_i32(i8 %a) {
; CHECK-LABEL: @zext_i8_i32
; CHECK-NEXT:   %1 = and i32 %a, 255
; CHECK-NEXT:   ret i32 %1
    %1 = zext i8 %a to i32
    ret i32 %1
}

define i64 @zext_i8_i64(i8 %a) {
; CHECK-LABEL: @zext_i8_i64
; CHECK-NEXT:   %1 = and i32 %a, 255
; CHECK-NEXT:   %2 = zext i32 %1 to i64
; CHECK-NEXT:   ret i64 %2
    %1 = zext i8 %a to i64
    ret i64 %1
}

define i64 @zext_i32_i64(i32 %a) {
; CHECK-LABEL: @zext_i32_i64
; CHECK-NEXT:   %1 = zext i32 %a to i64
; CHECK-NEXT:   ret i64 %1
    %1 = zext i32 %a to i64
    ret i64 %1
}

define i64 @zext_i44_i64(i44 %a) {
; CHECK-LABEL: @zext_i44_i64
; CHECK-NEXT: %1 = and i64 %a, 17592186044415
; CHECK-NEXT: ret i64 %1
    %1 = zext i44 %a to i64
    ret i64 %1
}

define i128 @zext_i64_i128(i64 %a) {
; CHECK-LABEL: @zext_i64_i128
; CHECK: store i64 %a, ptr
; CHECK: add i64 {{.*}}, 8
; CHECK: store i64 0
; CHECK-NEXT: ret void
    %1 = zext i64 %a to i128
    ret i128 %1
}