; RUN: watever -l 0 --legal %s | FileCheck %s

declare void @a()
declare void @b()

define void @if_else(i1 %a) {
; CHECK-LABEL: @if_else
; CHECK: trunc i32 
; CHECK-NEXT: br i1
entry:
  br i1 %a, label %then, label %else
then:
  call void @a()
  br label %merge
else:
  call void @b()
  br label %merge
merge:
  ret void
}

define void @loop(i1 %a) {
; CHECK-LABEL: @loop
; CHECK: trunc i32
; CHECK-NEXT: br
entry:
  br label %header
header:
  br i1 %a, label %exit, label %body
body:
  br label %header
exit:
  ret void
}