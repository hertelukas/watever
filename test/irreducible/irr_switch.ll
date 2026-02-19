; RUN: watever %s -o %t.o
; RUN: /opt/wasi-sdk/bin/clang %t.o -o %t.wasm
; RUN: wasmtime %t.wasm | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-wasip2"

declare i32 @printf(ptr, ...)

@format = private constant [4 x i8] c"%d\0A\00"
@bye = private constant [5 x i8] c"bye\0A\00"

define i32 @__main_argc_argv(i32 noundef %0, ptr noundef readnone captures(none) %1) local_unnamed_addr #0 {
entry:
  call i32 (ptr, ...) @printf(ptr @format, i32 %0)
  switch i32 %0, label %exit [ i32 0, label %h0
                               i32 1, label %h1
			       i32 2, label %h2 ]

h0:
  call i32 (ptr, ...) @printf(ptr @format, i32 0)
  %two = add i32 %0, 1
  switch i32 %two, label %exit [ i32 0, label %h0
                                 i32 1, label %h1
			         i32 2, label %h2 ]

h1:
  call i32 (ptr, ...) @printf(ptr @format, i32 1)
  %zero = sub i32 %0, 1
  switch i32 %zero, label %exit [ i32 0, label %h0
                                  i32 1, label %h1
		                  i32 2, label %h2 ]
h2:
  call i32 (ptr, ...) @printf(ptr @format, i32 2)
  %six = add i32 %0, 5
  switch i32 %six, label %exit [ i32 0, label %h0
                                 i32 1, label %h1
		                 i32 2, label %h2 ]
  

exit:
  call i32(ptr, ...) @printf(ptr @bye)
  ret i32 0
}

; CHECK: 1
; CHECK-NEXT: 1
; CHECK-NEXT: 0
; CHECK-NEXT: 2
; CHECK-NEXT: bye