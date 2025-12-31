; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-unknown"

; Uses i8
define void @store_i7(ptr %p, i7 %a) {
; CHECK-LABEL: $store_i7
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i32.const 127
; CHECK-NEXT:    i32.and
; CHECK-NEXT:    i32.store8
entry:
  store i7 %a, ptr %p
  ret void
}

define void @store_const_i7(ptr %p) {
; CHECK-LABEL: $store_const_i7
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    i32.const 1
; CHECK-NEXT:    i32.store8
entry:
  store i7 1, ptr %p
  ret void
}

define void @store_i8(ptr %p, i8 %a) {
; CHECK-LABEL: $store_i8
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i32.store8
entry:
  store i8 %a, ptr %p
  ret void
}

define void @store_const_i8(ptr %p) {
; CHECK-LABEL: $store_const_i8
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    i32.const 1
; CHECK-NEXT:    i32.store8 
entry:
  store i8 1, ptr %p
  ret void
}

; Uses i16
define void @store_i10(ptr %p, i10 %a) {
; CHECK-LABEL: $store_i10
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i32.const 1023
; CHECK-NEXT:    i32.and
; CHECK-NEXT:    i32.store16
entry:
  store i10 %a, ptr %p
  ret void
}

define void @store_const_i10(ptr %p) {
; CHECK-LABEL: $store_const_i10
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    i32.const 1
; CHECK-NEXT:    i32.store16
entry:
  store i10 1, ptr %p
  ret void
}

; Uses i16
define void @store_i16(ptr %p, i16 %a) {
; CHECK-LABEL: $store_i16
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i32.store16
entry:
  store i16 %a, ptr %p
  ret void
}

; Uses i16 + i8
define void @store_i22(ptr %p, i22 %a) {
; CHECK-LABEL: $store_i22
; CHECK-NEXT:    (local i32)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i32.const 4194303
; CHECK-NEXT:    i32.and
; CHECK-NEXT:    local.tee 2
; CHECK-NEXT:    i32.store16
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 2
; CHECK-NEXT:    i32.const 16
; CHECK-NEXT:    i32.shr_u
; CHECK-NEXT:    i32.store8 offset=2
entry:
  store i22 %a, ptr %p
  ret void
}

; Uses i32
define void @store_i25(ptr %p, i25 %a) {
; CHECK-LABEL: $store_i25
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i32.const 33554431
; CHECK-NEXT:    i32.and
; CHECK-NEXT:    i32.store
entry:
  store i25 %a, ptr %p
  ret void
}

define void @store_const_i25(ptr %p) {
; CHECK-LABEL: $store_const_i25
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    i32.const 1
; CHECK-NEXT:    i32.store
entry:
  store i25 1, ptr %p
  ret void
}

define void @store_i32(ptr %p, i32 %a) {
; CHECK-LABEL: $store_i32
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i32.store
entry:
  store i32 %a, ptr %p
  ret void
}

; Uses i32 + i8 = i40
define void @store_i33(ptr %p, i33 %a) {
; CHECK-LABEL: $store_i33
; CHECK-NEXT:    (local i64)
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i64.const 8589934591
; CHECK-NEXT:    i64.and
; CHECK-NEXT:    local.tee 2
; CHECK-NEXT:    i64.store32
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 2
; CHECK-NEXT:    i64.const 32
; CHECK-NEXT:    i64.shr_u
; CHECK-NEXT:    i64.store8 offset=4
entry:
  store i33 %a, ptr %p
  ret void
}

; Uses i32 + i8 = i40
define void @store_i40(ptr %p, i40 %a) {
; CHECK-LABEL: $store_i40
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i64.store32
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i64.const 32
; CHECK-NEXT:    i64.shr_u
; CHECK-NEXT:    i64.store8 offset=4
entry:
  store i40 %a, ptr %p
  ret void
}

define void @store_const_i40(ptr %p) {
; CHECK-LABEL: $store_const_i40
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    i32.const 1
; CHECK-NEXT:    i32.store align=1
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    i32.const 0
; CHECK-NEXT:    i32.store8 offset=4
entry:
  store i40 1, ptr %p
  ret void
}

; Uses i32 + i16 = i48
define void @store_i44(ptr %p, i44 %a) {
; CHECK-LABEL: $store_i44
; CHECK:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i64.const 17592186044415
; CHECK-NEXT:    i64.and
; CHECK-NEXT:    local.tee 2
; CHECK-NEXT:    i64.store32
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 2
; CHECK-NEXT:    i64.const 32
; CHECK-NEXT:    i64.shr_u
; CHECK-NEXT:    i64.store16 offset=4 
entry:
  store i44 %a, ptr %p
  ret void
}

; Uses i32 + i16 = i48
define void @store_i48(ptr %p, i48 %a) {
; CHECK-LABEL: $store_i48
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i64.store32 
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i64.const 32
; CHECK-NEXT:    i64.shr_u
; CHECK-NEXT:    i64.store16 offset=4 
entry:
  store i48 %a, ptr %p
  ret void
}

; Uses i32 + i16 + i8 = i56
define void @store_i49(ptr %p, i49 %a) {
; CHECK-LABEL: $store_i49
; CHECK:         local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i64.const 562949953421311
; CHECK-NEXT:    i64.and
; CHECK-NEXT:    local.tee 3
; CHECK-NEXT:    i64.store32 
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.tee 2
; CHECK-NEXT:    local.get 3
; CHECK-NEXT:    i64.const 32
; CHECK-NEXT:    i64.shr_u
; CHECK-NEXT:    i64.store16 offset=4 
; CHECK-NEXT:    local.get 2
; CHECK-NEXT:    local.get 3
; CHECK-NEXT:    i64.const 48
; CHECK-NEXT:    i64.shr_u
; CHECK-NEXT:    i64.store8 offset=6
entry:
  store i49 %a, ptr %p
  ret void
}

; Uses i64
define void @store_i57(ptr %p, i57 %a) {
; CHECK-LABEL: $store_i57
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i64.const 144115188075855871
; CHECK-NEXT:    i64.and
; CHECK-NEXT:    i64.store
entry:
  store i57 %a, ptr %p
  ret void
}

; Uses i64
define void @store_i64(ptr %p, i64 %a) {
; CHECK-LABEL: $store_i64
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i64.store
entry:
  store i64 %a, ptr %p
  ret void
}

define void @store_float(ptr %p, float %a) {
; CHECK-LABEL: $store_float
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    f32.store
entry:
  store float %a, ptr %p
  ret void
}

define void @store_double(ptr %p, double %a) {
; CHECK-LABEL: $store_double
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    f64.store
entry:
  store double %a, ptr %p
  ret void
}

define void @store_ptr(ptr %p, ptr %a) {
; CHECK-LABEL: $store_ptr
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    i32.store
entry:
  store ptr %a, ptr %p
  ret void
}

