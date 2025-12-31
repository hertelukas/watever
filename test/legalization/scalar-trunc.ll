; RUN: watever -l 0 --legal %s | FileCheck %s

define i8 @trunc_i32_i8(i32 %a) {
; CHECK-LABEL: @trunc_i32_i8
; CHECK-NOT: trunc
    %1 = trunc i32 %a to i8
    ret i8 %1
}

define i8 @trunc_i64_i8(i64 %a) {
; CHECK-LABEL: @trunc_i64_i8
; CHECK: trunc i64 {{.*}} to i32
    %1 = trunc i64 %a to i8
    ret i8 %1
}

define i32 @trunc_i64_i32(i64 %a) {
; CHECK-LABEL: @trunc_i64_i32
; CHECK: trunc i64 {{.*}} to i32
    %1 = trunc i64 %a to i32
    ret i32 %1
}

define i44 @trunc_i64_i44(i64 %a) {
; CHECK-LABEL: @trunc_i64_i44
; CHECK-NOT: trunc
    %1 = trunc i64 %a to i44
    ret i44 %1
}