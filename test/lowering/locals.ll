; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-unknown"

declare void @sink()

; ------------------------------------------------------------------------------
; Inter-type sorting
; ------------------------------------------------------------------------------

; f32 group should get the lower local indices (appear first in the header)
; because f32 has 3 uses vs i32 having 2 uses.
define void @test_inter_type_sorting_f32_most_frequent(ptr %ptr) {
; CHECK-LABEL: $test_inter_type_sorting_f32_most_frequent
; CHECK-NEXT: (local f32 i32)
entry:
  %a = load volatile float, ptr %ptr
  %d = load volatile i32, ptr %ptr
  call void @sink()
  br label %consumer

consumer:
  store volatile float %a, ptr %ptr
  store volatile float %a, ptr %ptr
  store volatile i32 %d, ptr %ptr
  ret void
}

; i32 group should get the lower local indices (appear first in the preamble)
; because i32 has 3 uses vs f32 having 2 uses.
define void @test_inter_type_sorting_i32_most_frequent(ptr %ptr) {
; CHECK-LABEL: $test_inter_type_sorting_i32_most_frequent
; CHECK-NEXT: (local i32 f32)
entry:
  %a = load volatile float, ptr %ptr
  %d = load volatile i32, ptr %ptr
  call void @sink()
  br label %consumer

consumer:
  store volatile float %a, ptr %ptr
  store volatile i32 %d, ptr %ptr
  store volatile i32 %d, ptr %ptr
  ret void
}

; ------------------------------------------------------------------------------
; Intra-type sorting
; ------------------------------------------------------------------------------

; Both are i32. %first has 3 uses, %second has 2 uses.
; %first should be assigned local 1, %second should be assigned local 2.
; The first load targets %first (local 1), the second targets %second (local 2).
define void @test_intra_type_sorting_first_most_frequent(ptr %ptr) {
; CHECK-LABEL: $test_intra_type_sorting_first_most_frequent
; CHECK: local.set 1
; CHECK: local.set 2
entry:
  %first = load volatile i32, ptr %ptr
  %second = load volatile i32, ptr %ptr
  call void @sink()
  br label %consumer

consumer:
  store volatile i32 %first, ptr %ptr
  store volatile i32 %first, ptr %ptr
  store volatile i32 %second, ptr %ptr
  ret void
}

; Both are i32. %second has 3 uses, %first has 2 uses.
; %second should be assigned local 1, %first should be assigned local 2.
; The first load targets %first (local 2), the second targets %second (local 1).
define void @test_intra_type_sorting_second_most_frequent(ptr %ptr) {
; CHECK-LABEL: $test_intra_type_sorting_second_most_frequent
; CHECK: local.set 2
; CHECK: local.set 1
entry:
  %first = load volatile i32, ptr %ptr
  %second = load volatile i32, ptr %ptr
  call void @sink()
  br label %consumer

consumer:
  store volatile i32 %first, ptr %ptr
  store volatile i32 %second, ptr %ptr
  store volatile i32 %second, ptr %ptr
  ret void
}