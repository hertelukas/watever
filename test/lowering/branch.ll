; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

define void @foo() {
  ret void
}

define void @bar() {
  ret void
}

define void @if_else(i1 %a) {
; CHECK-LABEL:   (func $if_else {{.*}} (param i32)
; CHECK: local.get 0
; CHECK-NEXT: i32.const 1
; CHECK-NEXT: i32.and
; CHECK-NEXT: if
; CHECK-NEXT: call $bar
; CHECK-NEXT: br 0
; CHECK-NEXT: else
; CHECK-NEXT: call $foo
; CHECK-NEXT: br 0
entry:
  br i1 %a, label %then, label %else
then:
  call void @bar()
  br label %merge
else:
  call void @foo()
  br label %merge
merge:
  ret void
}

define void @loop(i1 %a) {
; CHECK-LABEL:   (func $loop {{.*}} (param i32)
; CHECK: loop
; CHECK-NEXT: local.get 0
; CHECK-NEXT: i32.const
; CHECK-NEXT: i32.and
; CHECK-NEXT: if
; CHECK-NEXT: return
; CHECK-NEXT: else
; CHECK-NEXT: br 0
entry:
  br label %header
header:
  br i1 %a, label %exit, label %body
body:
  br label %header
exit:
  ret void
}

define i32 @live_across_blocks(i32 %a, i32 %b, i1 %cond) {
; CHECK-LABEL: live_across_blocks
; CHECK: local.get
; CHECK: local.get
; CHECK: i32.add
; CHECK: local.set
; CHECK: if
; CHECK: else
; CHECK: end
; CHECK-NOT: i32.add
entry:
    %x = add i32 %a, %b
    br i1 %cond, label %if, label %else
if:
    call void @foo()
    br label %exit
else:
    call void @bar()
    br label %exit
exit:
    %y = sub i32 %x, %b
    ret i32 %y
}


; The loop is not a merge node, even though it has multiple incoming edges
define void @forward_merge(i1 %a) {
; CHECK-LABEL: forward_merge
; CHECK: loop
; CHECK-NEXT: call $foo
; CHECK: if
; CHECK-NEXT: br 0
; CHECK: else
; CHECK-NEXT: br 1
entry:
  br i1 %a, label %merge, label %loop
loop:
  call void @foo()
  br i1 %a, label %loop, label %merge
merge:
  call void @bar()
  ret void
}