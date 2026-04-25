; RUN: watever %s -o %t.o
; RUN: wasm2wat %t.o | FileCheck %s

define void @foo(i32 %val) {
; CHECK-LABEL: $foo
; CHECK-NOT: br_table
entry:
  switch i32 %val, label %a [ i32 0, label %b ]
a:
  br label %b
b:
  br label %a
}
