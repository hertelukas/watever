; RUN: watever %s -o %t.o
; RUN: wasm2wat %t.o | FileCheck %s


define void @foo(i32 %val) {
; CHECK-LABEL: $foo
entry:
  switch i32 %val, label %a [ i32 0, label %b
                              i32 1, label %c
			      i32 2, label %d ]
a:
  br label %b
b:
  br label %c
c:
  br label %a
d:
  ret void
}
