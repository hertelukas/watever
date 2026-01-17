; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

declare float @llvm.fmuladd.f32(float %a, float %b, float %c)

define float @fmuladd(float %a, float %b, float %c) {
; CHECK-LABEL: $fmuladd
; CHECK: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: f32.mul
; CHECK-NEXT: local.get 2
; CHECK-NEXT: f32.add
  %x = call float @llvm.fmuladd.f32(float %a, float %b, float %c)
  ret float %x
}