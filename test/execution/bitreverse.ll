; RUN: watever %s -o %t.o
; RUN: /opt/wasi-sdk/bin/clang %t.o -o %t.wasm
; RUN: wasmtime %t.wasm | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-wasip2"

@format = private constant [6 x i8] c"%lld\0A\00"
declare i32 @printf(ptr, ...)

define i20 @test_bitreverse_i20(i20 %i) {
  %res = call i20 @llvm.bitreverse.i20(i20 %i)
  ret i20 %res
}

define i32 @test_bitreverse_i32(i32 %i) {
  %res = call i32 @llvm.bitreverse.i32(i32 %i)
  ret i32 %res
}

define i50 @test_bitreverse_i50(i50 %i) {
  %res = call i50 @llvm.bitreverse.i50(i50 %i)
  ret i50 %res
}

define i51 @test_bitreverse_i51(i51 %i) {
  %res = call i51 @llvm.bitreverse.i51(i51 %i)
  ret i51 %res
}

define i64 @test_bitreverse_i64(i64 %i) {
  %res = call i64 @llvm.bitreverse.i64(i64 %i)
  ret i64 %res
}

define i32 @__main_argc_argv(i32 noundef %0, ptr noundef readnone captures(none) %1) local_unnamed_addr #0 {
entry:
  %i20 = call i20 @test_bitreverse_i20(i20 881308)
  call i32 (ptr, ...) @printf(ptr @format, i20 %i20)

  %i32 = call i32 @test_bitreverse_i32(i32 306150430)
  call i32 (ptr, ...) @printf(ptr @format, i32 %i32)

  %i50 = call i50 @test_bitreverse_i50(i50 668572419319499)
  call i32 (ptr, ...) @printf(ptr @format, i50 %i50)

  %i51 = call i51 @test_bitreverse_i51(i51 1938189557433765)
  call i32 (ptr, ...) @printf(ptr @format, i51 %i51)

  %i64 = call i64 @test_bitreverse_i64(i64 1036033003542192604)
  call i32 (ptr, ...) @printf(ptr @format, i64 %i64)
  ret i32 0
}

; CHECK: 234731
; CHECK: 2017393736
; CHECK: 929756376735769
; CHECK: 1456858304682555
; CHECK: 4288955101480814192