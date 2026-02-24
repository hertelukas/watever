; RUN: watever -l 0 --legal %s | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-unknown"

define i32 @add(ptr %ptr, i32 %a) {
; CHECK: define i32 @add
; CHECK-NEXT: [[LD:%[0-9]+]] = load i32, ptr %ptr
; CHECK-NEXT: [[ST:%[0-9]+]] = add i32 [[LD]], %a
; CHECK-NEXT: store i32 [[ST]], ptr %ptr
; CHECK-NEXT: ret i32 [[LD]]
  %old = atomicrmw add ptr %ptr, i32 %a acquire
  ret i32 %old
}

define i32 @sub(ptr %ptr, i32 %a) {
; CHECK: define i32 @sub
; CHECK-NEXT: [[LD:%[0-9]+]] = load i32, ptr %ptr
; CHECK-NEXT: [[ST:%[0-9]+]] = sub i32 [[LD]], %a
; CHECK-NEXT: store i32 [[ST]], ptr %ptr
; CHECK-NEXT: ret i32 [[LD]]
  %old = atomicrmw sub ptr %ptr, i32 %a acquire
  ret i32 %old
}

define i32 @and(ptr %ptr, i32 %a) {
; CHECK: define i32 @and
; CHECK-NEXT: [[LD:%[0-9]+]] = load i32, ptr %ptr
; CHECK-NEXT: [[ST:%[0-9]+]] = and i32 [[LD]], %a
; CHECK-NEXT: store i32 [[ST]], ptr %ptr
; CHECK-NEXT: ret i32 [[LD]]
  %old = atomicrmw and ptr %ptr, i32 %a acquire
  ret i32 %old
}

define i32 @xor(ptr %ptr, i32 %a) {
; CHECK: define i32 @xor
; CHECK-NEXT: [[LD:%[0-9]+]] = load i32, ptr %ptr
; CHECK-NEXT: [[ST:%[0-9]+]] = xor i32 [[LD]], %a
; CHECK-NEXT: store i32 [[ST]], ptr %ptr
; CHECK-NEXT: ret i32 [[LD]]
  %old = atomicrmw xor ptr %ptr, i32 %a acquire
  ret i32 %old
}