; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

define float @rint_float(float %x) {
; CHECK-LABEL: $rint_float
; CHECK: local.get 0
; CHECK-NEXT: f32.nearest
entry:
  %c = call float @llvm.rint.f32(float %x)
  ret float %c
}

define float @roundeven_float(float %x) {
; CHECK-LABEL: $roundeven_float
; CHECK: local.get 0
; CHECK-NEXT: f32.nearest
entry:
  %c = call float @llvm.roundeven.f32(float %x)
  ret float %c
}

define float @nearbyint_float(float %x) {
; CHECK-LABEL: $nearbyint_float
; CHECK: local.get 0
; CHECK-NEXT: f32.nearest
entry:
  %c = call float @llvm.nearbyint.f32(float %x)
  ret float %c
}

define double @rint_double(double %x) {
; CHECK-LABEL: $rint_double
; CHECK: local.get 0
; CHECK-NEXT: f64.nearest
entry:
  %c = call double @llvm.rint.f64(double %x)
  ret double %c
}

define double @roundeven_double(double %x) {
; CHECK-LABEL: $roundeven_double
; CHECK: local.get 0
; CHECK-NEXT: f64.nearest
entry:
  %c = call double @llvm.roundeven.f64(double %x)
  ret double %c
}

define double @nearbyint_double(double %x) {
; CHECK-LABEL: $nearbyint_double
; CHECK: local.get 0
; CHECK-NEXT: f64.nearest
entry:
  %c = call double @llvm.nearbyint.f64(double %x)
  ret double %c
}
