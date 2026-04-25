; RUN: watever %s -o %t.o
; RUN: wasm2wat %t.o | FileCheck %s

define void @foo() {
; CHECK-LABEL: $foo
; CHECK-NEXT: loop
; CHECK-NEXT: br 0
  br i1 false, label %1, label %3

1:
  %2 = phi ptr [ null, %3 ], [ null, %0 ]
  br label %3

3:
  br label %1
}