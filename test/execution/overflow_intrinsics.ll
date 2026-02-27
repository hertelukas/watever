; RUN: watever %s -o %t.o
; RUN: /opt/wasi-sdk/bin/clang %t.o -o %t.wasm
; RUN: wasmtime %t.wasm | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-wasip2"

declare i32 @printf(ptr, ...)
@format = private constant [4 x i8] c"%d\0A\00"

define i8 @umul_i8(i8 %a, i8 %b) {
  %1 = tail call { i8, i1 } @llvm.umul.with.overflow.i8(i8 %a, i8 %b)
  %2 = extractvalue { i8, i1 } %1, 0
  %3 = extractvalue { i8, i1 } %1, 1
  %4 = select i1 %3, i8 %a, i8 %2
  ret i8 %4
}

define i8 @smul_i8(i8 %a, i8 %b) {
  %1 = tail call { i8, i1 } @llvm.smul.with.overflow.i8(i8 %a, i8 %b)
  %2 = extractvalue { i8, i1 } %1, 0
  %3 = extractvalue { i8, i1 } %1, 1
  %4 = select i1 %3, i8 %a, i8 %2
  ret i8 %4
}

define i16 @umul_i16(i16 %a, i16 %b) {
  %1 = tail call { i16, i1 } @llvm.umul.with.overflow.i16(i16 %a, i16 %b)
  %2 = extractvalue { i16, i1 } %1, 0
  %3 = extractvalue { i16, i1 } %1, 1
  %4 = select i1 %3, i16 %a, i16 %2
  ret i16 %4
}

define i16 @smul_i16(i16 %a, i16 %b) {
  %1 = tail call { i16, i1 } @llvm.smul.with.overflow.i16(i16 %a, i16 %b)
  %2 = extractvalue { i16, i1 } %1, 0
  %3 = extractvalue { i16, i1 } %1, 1
  %4 = select i1 %3, i16 %a, i16 %2
  ret i16 %4
}

define i32 @umul_i32(i32 %a, i32 %b) {
  %1 = tail call { i32, i1 } @llvm.umul.with.overflow.i32(i32 %a, i32 %b)
  %2 = extractvalue { i32, i1 } %1, 0
  %3 = extractvalue { i32, i1 } %1, 1
  %4 = select i1 %3, i32 %a, i32 %2
  ret i32 %4
}

define i32 @smul_i32(i32 %a, i32 %b) {
  %1 = tail call { i32, i1 } @llvm.smul.with.overflow.i32(i32 %a, i32 %b)
  %2 = extractvalue { i32, i1 } %1, 0
  %3 = extractvalue { i32, i1 } %1, 1
  %4 = select i1 %3, i32 %a, i32 %2
  ret i32 %4
}


define i32 @__main_argc_argv(i32 noundef %0, ptr noundef readnone captures(none) %1) local_unnamed_addr #0 {
entry:
  ; --- 8-BIT TESTS ---

  ; No overflow 12 * 12 = 144
  %umul_i8 = call i8 @umul_i8(i8 12, i8 12)
  %umul_i8_ext = zext i8 %umul_i8 to i32
  call i32 (ptr, ...) @printf(ptr @format, i32 %umul_i8_ext)

  ; Overflow 17 * 17 = 289
  %umul_i8_overflow = call i8 @umul_i8(i8 17, i8 17)
  %umul_i8_ovf_ext = zext i8 %umul_i8_overflow to i32
  call i32 (ptr, ...) @printf(ptr @format, i32 %umul_i8_ovf_ext)

  ; No overflow 11 * 11 = 121
  %smul_i8 = call i8 @smul_i8(i8 11, i8 11)
  %smul_i8_ext = sext i8 %smul_i8 to i32
  call i32 (ptr, ...) @printf(ptr @format, i32 %smul_i8_ext)

  ; Overflow 12 * 12 = 144
  %smul_i8_overflow = call i8 @smul_i8(i8 12, i8 12)
  %smul_i8_ovf_ext = sext i8 %smul_i8_overflow to i32
  call i32 (ptr, ...) @printf(ptr @format, i32 %smul_i8_ovf_ext)
  
  ; No overflow -10 * 10 = -100
  %smul_i8_neg = call i8 @smul_i8(i8 -10, i8 10)
  %smul_i8_neg_ext = sext i8 %smul_i8_neg to i32
  call i32 (ptr, ...) @printf(ptr @format, i32 %smul_i8_neg_ext)
  
  ; Overflow -12 * 12 = -144
  %smul_i8_neg_overflow = call i8 @smul_i8(i8 -12, i8 12)
  %smul_i8_neg_overflow_ext = sext i8 %smul_i8_neg_overflow to i32
  call i32 (ptr, ...) @printf(ptr @format, i32 %smul_i8_neg_overflow_ext)

  ; --- 16-BIT TESTS ---
  
  ; No overflow 200 * 200 = 40000
  %umul_i16 = call i16 @umul_i16(i16 200, i16 200)
  %umul_i16_ext = zext i16 %umul_i16 to i32
  call i32 (ptr, ...) @printf(ptr @format, i32 %umul_i16_ext)

  ; Overflow 300 * 300 = 90000
  %umul_i16_overflow = call i16 @umul_i16(i16 300, i16 300)
  %umul_i16_ovf_ext = zext i16 %umul_i16_overflow to i32
  call i32 (ptr, ...) @printf(ptr @format, i32 %umul_i16_ovf_ext)

  ; No overflow 150 * 150 = 22500
  %smul_i16 = call i16 @smul_i16(i16 150, i16 150)
  %smul_i16_ext = sext i16 %smul_i16 to i32
  call i32 (ptr, ...) @printf(ptr @format, i32 %smul_i16_ext)

  ; Overflow 200 * 200 = 40000
  %smul_i16_overflow = call i16 @smul_i16(i16 200, i16 200)
  %smul_i16_ovf_ext = sext i16 %smul_i16_overflow to i32
  call i32 (ptr, ...) @printf(ptr @format, i32 %smul_i16_ovf_ext)

  ; No overflow -150 * 150 = -22500
  %smul_i16_neg = call i16 @smul_i16(i16 -150, i16 150)
  %smul_i16_neg_ext = sext i16 %smul_i16_neg to i32
  call i32 (ptr, ...) @printf(ptr @format, i32 %smul_i16_neg_ext)

  ; Overflow -200 * 200 = -40000
  %smul_i16_neg_overflow = call i16 @smul_i16(i16 -200, i16 200)
  %smul_i16_neg_ovf_ext = sext i16 %smul_i16_neg_overflow to i32
  call i32 (ptr, ...) @printf(ptr @format, i32 %smul_i16_neg_ovf_ext)

  ; --- 32-BIT TESTS ---
  
  ; No overflow 40000 * 40000 = 1600000000
  %umul_i32 = call i32 @umul_i32(i32 40000, i32 40000)
  call i32 (ptr, ...) @printf(ptr @format, i32 %umul_i32)

  ; Overflow 70000 * 70000 = 4900000000
  %umul_i32_overflow = call i32 @umul_i32(i32 70000, i32 70000)
  call i32 (ptr, ...) @printf(ptr @format, i32 %umul_i32_overflow)

  ; No overflow 30000 * 30000 = 900000000
  %smul_i32 = call i32 @smul_i32(i32 30000, i32 30000)
  call i32 (ptr, ...) @printf(ptr @format, i32 %smul_i32)

  ; Overflow 50000 * 50000 = 2500000000
  %smul_i32_overflow = call i32 @smul_i32(i32 50000, i32 50000)
  call i32 (ptr, ...) @printf(ptr @format, i32 %smul_i32_overflow)

  ; No overflow -30000 * 30000 = -900000000
  %smul_i32_neg = call i32 @smul_i32(i32 -30000, i32 30000)
  call i32 (ptr, ...) @printf(ptr @format, i32 %smul_i32_neg)

  ; Overflow -50000 * 50000 = -2500000000
  %smul_i32_neg_overflow = call i32 @smul_i32(i32 -50000, i32 50000)
  call i32 (ptr, ...) @printf(ptr @format, i32 %smul_i32_neg_overflow)

  ret i32 0
}

; CHECK: 144
; CHECK-NEXT: 17
; CHECK-NEXT: 121
; CHECK-NEXT: 12
; CHECK-NEXT: -100
; CHECK-NEXT: -12
; CHECK-NEXT: 40000
; CHECK-NEXT: 300
; CHECK-NEXT: 22500
; CHECK-NEXT: 200
; CHECK-NEXT: -22500
; CHECK-NEXT: -200
; CHECK-NEXT: 1600000000
; CHECK-NEXT: 70000
; CHECK-NEXT: 900000000
; CHECK-NEXT: 50000
; CHECK-NEXT: -900000000
; CHECK-NEXT: -50000