; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-unknown"

; Uses i8
define i7 @load_i7(ptr %a) {
; CHECK-LABEL:   $load_i7
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    i32.load8_u
; CHECK-NOT:     load
entry:
  %1 = load i7, ptr %a
  ret i7 %1
}

define i8 @load_i8(ptr %a) {
; CHECK-LABEL:   $load_i8
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    i32.load8_u
; CHECK-NOT:     load
entry:
  %1 = load i8, ptr %a
  ret i8 %1
}

; Uses i16
define i10 @load_i10(ptr %a) {
; CHECK-LABEL:   $load_i10
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    i32.load16_u
; CHECK-NOT:     load
entry:
  %1 = load i10, ptr %a
  ret i10 %1
}

; Uses i16
define i16 @load_i16(ptr %a) {
; CHECK-LABEL:   $load_i16
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    i32.load16_u 
; CHECK-NOT:     load
entry:
  %1 = load i16, ptr %a
  ret i16 %1
}

; Uses i16 + i8
define i22 @load_i22(ptr %a) {
; CHECK-LABEL:   $load_i22
; CHECK:         i32.load16_u
; CHECK:         i32.load8_u offset=2
; CHECK-NOT:     load
entry:
  %1 = load i22, ptr %a
  ret i22 %1
}

; Uses i32
define i25 @load_i25(ptr %a) {
; CHECK-LABEL:   $load_i25
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    i32.load
; CHECK-NOT:     load
entry:
  %1 = load i25, ptr %a
  ret i25 %1
}

define i32 @load_i32(ptr %a) {
; CHECK-LABEL:   $load_i32 
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    i32.load
; CHECK-NOT:     load
entry:
  %1 = load i32, ptr %a
  ret i32 %1
}

; Uses i32 + i8 = i40
define i33 @load_i33(ptr %a) {
; CHECK-LABEL:   $load_i33
; CHECK:         i64.load32_u
; CHECK:         i64.load8_u offset=4
; CHECK-NOT:     load
entry:
  %1 = load i33, ptr %a
  ret i33 %1
}

; Uses i32 + i8 = i40
define i40 @load_i40(ptr %a) {
; CHECK-LABEL:   $load_i40 
; CHECK:         i64.load32_u
; CHECK:         i64.load8_u offset=4
; CHECK-NOT:     load
entry:
  %1 = load i40, ptr %a
  ret i40 %1
}

; Uses i32 + i16 = i48
define i44 @load_i44(ptr %a) {
; CHECK-LABEL:   $load_i44 
; CHECK:         i64.load32_u
; CHECK:         i64.load16_u offset=4
; CHECK-NOT:     load
entry:
  %1 = load i44, ptr %a
  ret i44 %1
}

; Uses i32 + i16 = i48
define i48 @load_i48(ptr %a) {
; CHECK-LABEL:   $load_i48 
; CHECK:         i64.load32_u
; CHECK:         i64.load16_u offset=4
; CHECK-NOT:     load
entry:
  %1 = load i48, ptr %a
  ret i48 %1
}

; Uses i32 + i16 + i8 = i56
define i49 @load_i49(ptr %a) {
; CHECK-LABEL:   $load_i49 
; CHECK:         i64.load32_u
; CHECK:         i64.load16_u offset=4
; CHECK:         i64.load8_u offset=6
; CHECK-NOT:     load
entry:
  %1 = load i49, ptr %a
  ret i49 %1
}

; Uses i64
define i57 @load_i57(ptr %a) {
; CHECK-LABEL:   $load_i57
; CHECK:    local.get 0
; CHECK-NEXT:    i64.load
; CHECK-NOT:     load
entry:
  %1 = load i57, ptr %a
  ret i57 %1
}

; Uses i64
define i64 @load_i64(ptr %a) {
; CHECK-LABEL:   $load_i64 
; CHECK:    local.get 0
; CHECK-NEXT:    i64.load
; CHECK-NOT:     load
entry:
  %1 = load i64, ptr %a
  ret i64 %1
}

define float @load_float(ptr %a) {
; CHECK-LABEL:   $load_float
; CHECK:    local.get 0
; CHECK-NEXT:    f32.load 
; CHECK-NOT:     load
entry:
  %1 = load float, ptr %a
  ret float %1
}

define double @load_double(ptr %a) {
; CHECK-LABEL:   $load_double
; CHECK:    local.get 0
; CHECK-NEXT:    f64.load
; CHECK-NOT:     load
entry:
  %1 = load double, ptr %a
  ret double %1
}

define ptr @load_ptr(ptr %a) {
; CHECK-LABEL:   $load_ptr 
; CHECK:    local.get 0
; CHECK-NEXT:    i32.load
; CHECK-NOT:     load
entry:
  %1 = load ptr, ptr %a
  ret ptr %1
}

define ptr @load_root_gep(ptr %a) {
; CHECK-LABEL:  $load_root_gep
; CHECK: local.get 0
; CHECK-NEXT: i32.const 12
; CHECK-NEXT: i32.add
; CHECK-NEXT: local.tee 0
; CHECK-NEXT: i32.load
; CHECK-NEXT: local.set 1
; CHECK-NEXT: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: i32.store
; CHECK-NEXT: local.get 0
entry:
; %a is a root and dies here; so %1 can be stored into the same local
  %1 = getelementptr i8, ptr %a, i32 12
; the load here does a greedy inlining of the offset - thinking that %a
; is still valid 
  %2 = load i32, ptr %1, align 4
  store i32 %2, ptr %1
  br label %exit
exit:
  ret ptr %1
}

define i32 @write_after_read(ptr %ptr) {
; CHECK-LABEL: write_after_read
; CHECK:      local.get       0
; CHECK-NEXT: i32.load
; CHECK-NEXT: local.set       1
; CHECK-NEXT: local.get       0
; CHECK-NEXT: i32.const       1
; CHECK-NEXT: i32.store
; CHECK-NEXT: local.get       1
entry:
  %a = load i32, ptr %ptr
  store i32 1, ptr %ptr
  ret i32 %a
}