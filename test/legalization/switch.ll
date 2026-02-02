; RUN: watever -l 0 %s --legal | FileCheck %s

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
define void @dense(i32 %target) {
; CHECK-LABEL: define void @dense
; CHECK:      switch i32 %target, label %merge [
; CHECK-NEXT:   i32 0, label %a
; CHECK-NEXT:   i32 1, label %b
; CHECK-NEXT:   i32 2, label %c
; CHECK-NEXT: ]
  switch i32 %target, label %merge [ i32 0, label %a
                                     i32 1, label %b
                                     i32 2, label %c ]
a:
  call void @a()
  br label %merge
  
b:
  call void @b()
  br label %merge
  
c:
  call void @c()
  br label %merge

merge:
  ret void
}

define void @dense_high(i32 %target) {
; CHECK-LABEL: define void @dense_high
; CHECK:  [[COND:%[0-9]+]] = sub i32 %target, 100000
; CHECK-NEXT: switch i32 [[COND]], label %merge [
; CHECK-NEXT:   i32 0, label %a
; CHECK-NEXT:   i32 1, label %b
; CHECK-NEXT:   i32 2, label %c
; CHECK-NEXT: ]
  switch i32 %target, label %merge [ i32 100000, label %a
                                     i32 100001, label %b
				     i32 100002, label %c ]
a:
  call void @a()
  br label %merge

b:
  call void @b()
  br label %merge

c:
  call void @c()
  br label %merge

merge:
  ret void
}

define void @sparse(i32 %target) {
; CHECK-LABEL: define void @sparse
; CHECK:  %sw.pivot = icmp sle i32 %target, -1875074403
; CHECK-NEXT:  br i1 %sw.pivot, label %sw.left, label %sw.right

; CHECK: sw.left:
; CHECK-NEXT: [[LCOND:%[0-9]+]] = icmp eq i32 %target, -1875074403
; CHECK-NEXT: br i1 [[LCOND]], label %b, label %merge

; CHECK: sw.right:
; CHECK-NEXT: [[RCOND:%[0-9]+]] = icmp eq i32 %target, 1
; CHECK-NEXT: br i1 [[RCOND]], label %a, label %merge
  switch i32 %target, label %merge [ i32 1, label %a
                                     i32 -1875074403, label %b ]
a:
  call void @a()
  br label %merge

b:
  call void @b()
  br label %merge

merge:
  ret void
}

define void @sparse_i64(i64 %target) {
; CHECK-LABEL: define void @sparse_i64
  switch i64 %target, label %merge [ i64 1, label %a
                                     i64 2, label %b
				     i64 118750744033187, label %c
				     i64 118750744033188, label %c
				     i64 -18237912, label %a
				     i64 -18237913, label %b ]
a:
  call void @a()
  br label %merge

b:
  call void @b()
  br label %merge

c:
  call void @c()
  br label %merge

merge:
  ret void
}
