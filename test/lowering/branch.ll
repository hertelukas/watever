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