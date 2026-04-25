; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

define i1 @test_nothing(double %d) {
; CHECK-LABEL: test_nothing
; CHECK-NEXT:   i32.const 0
  %res = call i1 @llvm.is.fpclass.f64(double %d, i32 0)
  ret i1 %res
}

define i1 @test_zero(double %d) {
; CHECK-LABEL: test_zero
; CHECK-NEXT:   local.get 0
; CHECK-NEXT:   f64.const 0x0p+0
; CHECK-NEXT:   f64.eq
  %res = call i1 @llvm.is.fpclass.f64(double %d, i32 96)
  ret i1 %res
}

define i1 @test_snan(double %d) {
; CHECK-LABEL: $test_snan
; CHECK: local.get       0
; CHECK-NEXT: i64.reinterpret_f64
; CHECK-NEXT: i64.const       9223372036854775807
; CHECK-NEXT: i64.and
; CHECK-NEXT: local.tee       1
; CHECK-NEXT: i64.const       9218868437227405312
; CHECK-NEXT: i64.gt_s
; CHECK-NEXT: local.get       1
; CHECK-NEXT: i64.const       9221120237041090560
; CHECK-NEXT: i64.lt_s
; CHECK-NEXT: i32.and
  %res = call i1 @llvm.is.fpclass.f64(double %d, i32 1)
  ret i1 %res
}

define i1 @test_qnan(double %d) {
; CHECK-LABEL: $test_qnan
; CHECK: local.get       0
; CHECK-NEXT: i64.reinterpret_f64
; CHECK-NEXT: i64.const       9223372036854775807
; CHECK-NEXT: i64.and
; CHECK-NEXT: i64.const       9221120237041090559
; CHECK-NEXT: i64.gt_s
  %res = call i1 @llvm.is.fpclass.f64(double %d, i32 2)
  ret i1 %res
}

define i1 @test_positive_infinity(double %d) {
; CHECK-LABEL: test_positive_infinity
; CHECK-NEXT:   local.get 0
; CHECK-NEXT:   f64.const inf
; CHECK-NEXT:   f64.eq
  %res = call i1 @llvm.is.fpclass.f64(double %d, i32 512)
  ret i1 %res
}

define i1 @test_negative_infinity(double %d) {
; CHECK-LABEL: test_negative_infinity
; CHECK-NEXT:   local.get 0
; CHECK-NEXT:   f64.const -inf
; CHECK-NEXT:   f64.eq
  %res = call i1 @llvm.is.fpclass.f64(double %d, i32 4)
  ret i1 %res
}

;; 2^9 + 2^2 = 516
define i1 @test_infinity(double %d) {
; CHECK-LABEL: test_infinity
; CHECK-NEXT:   local.get 0
; CHECK-NEXT:   f64.abs
; CHECK-NEXT:   f64.const inf
; CHECK-NEXT:   f64.eq
  %res = call i1 @llvm.is.fpclass.f64(double %d, i32 516)
  ret i1 %res
}

define i1 @test_nothing_float(float %f) {
; CHECK-LABEL: test_nothing_float
; CHECK-NEXT:   i32.const 0
  %res = call i1 @llvm.is.fpclass.f32(float %f, i32 0)
  ret i1 %res
}

define i1 @test_zero_float(float %f) {
; CHECK-LABEL: test_zero_float
; CHECK-NEXT:   local.get 0
; CHECK-NEXT:   f32.const 0x0p+0
; CHECK-NEXT:   f32.eq
  %res = call i1 @llvm.is.fpclass.f32(float %f, i32 96)
  ret i1 %res
}

define i1 @test_snan_float(float %f) {
; CHECK-LABEL: $test_snan_float
; CHECK: local.get       0
; CHECK-NEXT: i32.reinterpret_f32
; CHECK-NEXT: i32.const       2147483647
; CHECK-NEXT: i32.and
; CHECK-NEXT: local.tee       1
; CHECK-NEXT: i32.const       2139095040
; CHECK-NEXT: i32.gt_s
; CHECK-NEXT: local.get       1
; CHECK-NEXT: i32.const       2143289344
; CHECK-NEXT: i32.lt_s
; CHECK-NEXT: i32.and
  %res = call i1 @llvm.is.fpclass.f32(float %f, i32 1)
  ret i1 %res
}

define i1 @test_qnan_float(float %f) {
; CHECK-LABEL: $test_qnan_float
; CHECK: local.get       0
; CHECK-NEXT: i32.reinterpret_f32
; CHECK-NEXT: i32.const       2147483647
; CHECK-NEXT: i32.and
; CHECK-NEXT: i32.const       2143289343
; CHECK-NEXT: i32.gt_s
  %res = call i1 @llvm.is.fpclass.f32(float %f, i32 2)
  ret i1 %res
}

define i1 @test_positive_infinity_float(float %f) {
; CHECK-LABEL: test_positive_infinity_float
; CHECK-NEXT:   local.get 0
; CHECK-NEXT:   f32.const inf
; CHECK-NEXT:   f32.eq
  %res = call i1 @llvm.is.fpclass.f32(float %f, i32 512)
  ret i1 %res
}

define i1 @test_negative_infinity_float(float %f) {
; CHECK-LABEL: test_negative_infinity_float
; CHECK-NEXT:   local.get 0
; CHECK-NEXT:   f32.const -inf
; CHECK-NEXT:   f32.eq
  %res = call i1 @llvm.is.fpclass.f32(float %f, i32 4)
  ret i1 %res
}

;; 2^9 + 2^2 = 516
define i1 @test_infinity_float(float %f) {
; CHECK-LABEL: test_infinity_float
; CHECK-NEXT:   local.get 0
; CHECK-NEXT:   f32.abs
; CHECK-NEXT:   f32.const inf
; CHECK-NEXT:   f32.eq
  %res = call i1 @llvm.is.fpclass.f32(float %f, i32 516)
  ret i1 %res
}
