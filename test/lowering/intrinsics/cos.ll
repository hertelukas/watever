; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

declare float @llvm.cos.f32(float)
declare double @llvm.cos.f64(double)

define float @fcos(float %x) {
; CHECK-LABEL: $fcos
; CHECK: local.get 0
; CHECK-NEXT: call
entry:
  %c = call float @llvm.cos.f32(float %x)
  ret float %c
}

define double @dcos(double %x) {
; CHECK-LABEL: $dcos
; CHECK: local.get 0
; CHECK-NEXT: call
entry:
  %c = call double @llvm.cos.f64(double %x)
  ret double %c
}
