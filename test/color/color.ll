; RUN: watever -l 5 %s -o /dev/zero | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-unknown"

declare void @sink(i32 %val)
declare i32 @start()

define i32 @reuse_simple() {
; CHECK-LABEL: Coloring function reuse_simple
; CHECK: Coloring block entry
; CHECK-NEXT: Mapping x to local 0
; CHECK-NEXT: Coloring block next
; CHECK-NEXT: Coloring block exit
; CHECK-NEXT: Mapping z to local 0
entry:
  %x = call i32 @start()
  br label %next

next:
  %y = add i32 %x, 2
  call void @sink(i32 %y)
  br label %exit

exit:
  %z = call i32 @start()
  call void @sink(i32 %z)
  ret i32 %z
}

define i32 @arg_reuse(i32 %arg) {
; CHECK-LABEL: Coloring function arg_reuse
; CHECK: Coloring block entry
; CHECK-NEXT: Mapping x to local 0
; CHECK-NEXT: Coloring block next
; CHECK-NEXT: Coloring block exit
; CHECK-NEXT: Mapping z to local 0
entry:
  %x = call i32 @start()
  br label %next

next:
  %y = add i32 %x, 2
  call void @sink(i32 %y)
  br label %exit

exit:
  %z = call i32 @start()
  call void @sink(i32 %z)
  ret i32 %z
}

define i32 @no_arg_reuse(i32 %arg) {
; CHECK-LABEL: Coloring function no_arg_reuse
; CHECK: Coloring block entry
; CHECK-NEXT: Mapping x to local 1
; CHECK-NEXT: Coloring block next
; CHECK-NEXT: Coloring block exit
; CHECK-NEXT: Mapping z to local 1
entry:
  %x = call i32 @start()
  br label %next

next:
  %y = add i32 %x, 2
  call void @sink(i32 %y)
  br label %exit

exit:
  %z = call i32 @start()
  call void @sink(i32 %z)
  call void @sink(i32 %arg)
  ret i32 %z
}

define i32 @promoted_alloca(i1 %cond, i32 %val) {
; CHECK-LABEL: Coloring function promoted_alloca
;
; CHECK: Coloring block entry
; CHECK-NEXT: Mapping promoted ptr2 to local 2
;
; CHECK-NEXT: Coloring block head
; CHECK-NEXT: Mapping promoted ptr1 to local 1
entry:
  %ptr1 = alloca i32
  %ptr2 = alloca i32
  store i32 %val, ptr %ptr2
  br label %head

head:
  br i1 %cond, label %if, label %else

if:
  store i32 2, ptr %ptr1
  br label %merge

else:
  %0 = load i32, ptr %ptr2
  store i32 %0, ptr %ptr1
  br label %merge

merge:
  %res = load i32, ptr %ptr1
  ret i32 %res
}