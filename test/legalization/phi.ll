; RUN: watever -l 0 %s --legal | FileCheck %s

define i32 @foo() {
    ret i32 100
}

define i32 @bar() {
    ret i32 200
}

define i32 @phi_i32(i1 %cond) {
; CHECK-LABEL: @phi_i32
; CHECK: phi i32 [ {{.*}}, %if ], [ {{.*}}, %else ]
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
; CHECK-LABEL: @phi_i128
; CHECK: phi i64
; CHECK: phi i64
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

define i32 @phi_loop(i32 %a, i1 %cond) {
; CHECK-LABEL: @phi_loop
; CHECK: phi i32 [ %a, %entry ], [ %1, %header ]
entry:
    br label %header
header:
    %1 = phi i32 [%a, %entry], [%2, %header]
    %2 = add i32 %1, 1
    br i1 %cond, label %header, label %exit
exit:
    ret i32 %2
}

define void @phi_i1(i32 %a, i32 %b) {
; CHECK-LABEL: @phi_i1
; CHECK: phi i32
entry:
  br label %header
header:
  %1 = phi i1 [false, %entry], [%cond, %header]
  %cond = icmp eq i32 %a, %b
  br i1 %cond, label %header, label %exit
exit:
  ret void
}