; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

; TODO write actual tests

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-unknown"

define i32 @foo() {
    ret i32 100
}

define i32 @bar() {
    ret i32 200
}

define void @f(i32 %a) {
    ret void
}

define i32 @phi_i32(i1 %cond) {
; CHECK-LABEL: $phi_i32
; CHECK: local.get
entry:
    br i1 %cond, label %if, label %else
if:
    %1 = call i32 @foo()
    br label %exit
else:
    %2 = call i32 @bar()
    br label %exit
exit:
    %3 = phi i32 [%1, %if], [%2, %else]
    ret i32 %3
}

define i128 @phi_i128(i128 %a, i128 %b, i1 %cond) {
; CHECK-LABEL: $phi_i128
; CHECK: local.get
entry:
    br i1 %cond, label %if, label %else
if:
    %1 = add i128 %a, 1
    br label %exit
else:
    %2 = add i128 %b, 2
    br label %exit
exit:
    %3 = phi i128 [%1, %if], [%2, %else]
    ret i128 %3
}

define void @phi_loop_not_critical(i32 %a, i1 %cond) {
; CHECK-LABEL: phi_loop
; CHECK: local.get
entry:
    br label %header
header:
    %1 = phi i32 [%a, %entry], [%2, %pre-header]
    %2 = add i32 %1, 1
    br i1 %cond, label %pre-header, label %exit
pre-header:
    br label %header
exit:
    call void @f(i32 %2)
    ret void
}
