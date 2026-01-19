; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

define void @a() {
  ret void
}
define void @b() {
  ret void
}
define void @c() {
  ret void
}

; Could also be any other order
define void @table(i32 %target) {
; CHECK-LABEL $table
; CHECK: local.get 0
; CHECK-NEXT: br_table 2 {{.*}} 1 {{.*}} 0 {{.*}} 3
; CHECK: call $c
; CHECK: call $b
; CHECK: call $a
  switch i32 %target, label %merge [ i32 0, label %onzero
                                     i32 1, label %onone
                                     i32 2, label %ontwo ]
onzero:
  call void @a()
  br label %merge
  
onone:
  call void @b()
  br label %merge
  
ontwo:
  call void @c()
  br label %merge

merge:
  ret void
}

define void @sparse(i32 %target) {
; CHECK-LABEL $sparse
; CHECK: local.get 0
; Default is 3, 0 is b(), 1 is a(), 2 is c()
; DEFAULT, A, DEFAULT, DEFAULT, B, DEFAULT, DEFAULT, DEFAULT, B

; CHECK-NEXT: br_table 3 {{.*}} 1 {{.*}} 3 {{.*}} 3 {{.*}} 0 {{.*}} 3 {{.*}} 3 {{.*}} 3 {{.*}} 2 {{.*}} 3

  switch i32 %target, label %merge [ i32 8, label %oneight
                                     i32 1, label %onone
				     i32 4, label %onfour ]
onone:
  call void @a()
  br label %merge

onfour:
  call void @b()
  br label %merge

oneight:
  call void @c()
  br label %merge

merge:
  ret void
}