; RUN: watever -l 0 --legal %s | FileCheck %s

; Uses i8
define void @store_i7(ptr %p, i7 %a) {
; CHECK-LABEL: @store_i7
; CHECK: and i32 {{.*}} 127
; CHECK: store i8
entry:
  store i7 %a, ptr %p
  ret void
}

define void @store_i8(ptr %p, i8 %a) {
; CHECK-LABEL: @store_i8
; CHECK-NOT: and
; CHECK: store i8
entry:
  store i8 %a, ptr %p
  ret void
}

; Uses i16
define void @store_i10(ptr %p, i10 %a) {
; CHECK-LABEL: @store_i10
; CHECK: and i32 {{.*}} 1023
; CHECK: store i16
entry:
  store i10 %a, ptr %p
  ret void
}

; Uses i16
define void @store_i16(ptr %p, i16 %a) {
; CHECK-LABEL: @store_i16
; CHECK-NOT: and
; CHECK: store i16
entry:
  store i16 %a, ptr %p
  ret void
}

; Uses i16 + i8
define void @store_i22(ptr %p, i22 %a) {
; CHECK-LABEL: @store_i22
; CHECK: and i32 {{.*}} 4194303
; CHECK: store i16
; CHECK: store i8
entry:
  store i22 %a, ptr %p
  ret void
}

; Uses i32
define void @store_i25(ptr %p, i25 %a) {
; CHECK-LABEL: @store_i25
; CHECK: and i32 {{.*}} 33554431
; CHECK: store i32
; CHECK-NOT: store
entry:
  store i25 %a, ptr %p
  ret void
}

define void @store_i32(ptr %p, i32 %a) {
; CHECK-LABEL: @store_i32
; CHECK-NOT: and
; CHECK: store i32
; CHECK-NOT: store
entry:
  store i32 %a, ptr %p
  ret void
}

; Uses i32 + i8 = i40
define void @store_i33(ptr %p, i33 %a) {
; CHECK-LABEL: @store_i33
; CHECK: and {{.*}} 8589934591
; CHECK: store i32
; CHECK: store i8
entry:
  store i33 %a, ptr %p
  ret void
}

; Uses i32 + i8 = i40
define void @store_i40(ptr %p, i40 %a) {
; CHECK-LABEL: @store_i40
; CHECK-NOT: and
; CHECK: store i32
; CHECK: store i8
entry:
  store i40 %a, ptr %p
  ret void
}

; Uses i32 + i16 = i48
define void @store_i44(ptr %p, i44 %a) {
; CHECK-LABEL: @store_i44
; CHECK: and {{.*}} 17592186044415
; CHECK: store i32
; CHECK: store i16
; CHECK-NOT: store
entry:
  store i44 %a, ptr %p
  ret void
}

; Uses i32 + i16 = i48
define void @store_i48(ptr %p, i48 %a) {
; CHECK-LABEL: @store_i48
; CHECK-NOT: and
; CHECK: store i32
; CHECK: lshr i64
; CHECK: store i16
; CHECK-NOT: store 
entry:
  store i48 %a, ptr %p
  ret void
}

; Uses i32 + i16 + i8 = i56
define void @store_i49(ptr %p, i49 %a) {
; CHECK-LABEL: @store_i49
; CHECK: and {{.*}} 562949953421311
; CHECK: store i32
; CHECK: store i16
; CHECK: store i8
entry:
  store i49 %a, ptr %p
  ret void
}

; Uses i64
define void @store_i57(ptr %p, i57 %a) {
; CHECK-LABEL: @store_i57
; CHECK: and {{.*}} 144115188075855871
; CHECK: store i64
; CHECK-NOT: store
entry:
  store i57 %a, ptr %p
  ret void
}

; Uses i64
define void @store_i64(ptr %p, i64 %a) {
; CHECK-LABEL: @store_i64
; CHECK-NOT: and
; CHECK: store i64
; CHECK-NOT: store
entry:
  store i64 %a, ptr %p
  ret void
}

define void @store_float(ptr %p, float %a) {
; CHECK-LABEL: define void @store_float(ptr %p
; CHECK: store float %a, ptr %p
entry:
  store float %a, ptr %p
  ret void
}

define void @store_double(ptr %p, double %a) {
; CHECK-LABEL: define void @store_double(ptr %p
; CHECK: store double %a, ptr %p
entry:
  store double %a, ptr %p
  ret void
}

define void @store_ptr(ptr %p, ptr %a) {
; CHECK-LABEL: define void @store_ptr(ptr %p, ptr %a)
; CHECK: store ptr %a, ptr %p
entry:
  store ptr %a, ptr %p
  ret void
}

