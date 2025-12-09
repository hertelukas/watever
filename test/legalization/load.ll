; RUN: watever -l 0 --legal %s | FileCheck %s

; Uses i8
define i7 @load_i7(ptr %a) {
; CHECK-LABEL: i32 @load_i7
; CHECK: load i8
; CHECK: zext i8 {{.*}} to i32
entry:
  %1 = load i7, ptr %a
  ret i7 %1
}

define i8 @load_i8(ptr %a) {
; CHECK-LABEL: i32 @load_i8
; CHECK: load i8
; CHECK-NEXT: zext i8 {{.*}} to i32
entry:
  %1 = load i8, ptr %a
  ret i8 %1
}

; Uses i16
define i10 @load_i10(ptr %a) {
; CHECK-LABEL: i32 @load_i10
; CHECK: load i16
; CHECK-NEXT: zext i16 {{.*}} to i32
entry:
  %1 = load i10, ptr %a
  ret i10 %1
}

; Uses i16
define i16 @load_i16(ptr %a) {
; CHECK-LABEL: i32 @load_i16
; CHECK: load i16
; CHECK-NEXT: zext i16 {{.*}} to i32
entry:
  %1 = load i16, ptr %a
  ret i16 %1
}

; Uses i16 + i8
define i22 @load_i22(ptr %a) {
; CHECK-LABEL: i32 @load_i22
; CHECK: load i16
; CHECK-NEXT: zext i16 {{.*}} to i32
; CHECK: load i8
; CHECK-NEXT: zext i8 {{.*}} to i32
; CHECK: or
entry:
  %1 = load i22, ptr %a
  ret i22 %1
}

; Uses i32
define i25 @load_i25(ptr %a) {
; CHECK-LABEL: @load_i25
; CHECK: load i32
entry:
  %1 = load i25, ptr %a
  ret i25 %1
}

define i32 @load_i32(ptr %a) {
; CHECK-LABEL: @load_i32
; CHECK: load i32
entry:
  %1 = load i32, ptr %a
  ret i32 %1
}

; Uses i32 + i8 = i40
define i33 @load_i33(ptr %a) {
; CHECK-LABEL: i64 @load_i33
; CHECK: load i32
; CHECK-NEXT: zext i32 {{.*}} to i64
; CHECK: load i8
; CHECK-NEXT: zext i8 {{.*}} to i64
entry:
  %1 = load i33, ptr %a
  ret i33 %1
}

; Uses i32 + i8 = i40
define i40 @load_i40(ptr %a) {
; CHECK-LABEL: i64 @load_i40
; CHECK: load i32
; CHECK-NEXT: zext i32 {{.*}} to i64
; CHECK: load i8
; CHECK-NEXT: zext i8 {{.*}} to i64
; CHECK: or
entry:
  %1 = load i40, ptr %a
  ret i40 %1
}

; Uses i32 + i16 = i48
define i44 @load_i44(ptr %a) {
; CHECK-LABEL: i64 @load_i44
; CHECK: load i32
; CHECK-NEXT: zext i32 {{.*}} to i64
; CHECK: load i16
; CHECK-NEXT: zext i16 {{.*}} to i64
; CHECK: or
entry:
  %1 = load i44, ptr %a
  ret i44 %1
}

; Uses i32 + i16 = i48
define i48 @load_i48(ptr %a) {
; CHECK-LABEL: @load_i48
; CHECK: load i32
; CHECK-NEXT: zext {{.*}} to i64
; CHECK: load i16
; CHECK-NEXT: zext {{.*}} to i64
; CHECK: or
entry:
  %1 = load i48, ptr %a
  ret i48 %1
}

; Uses i32 + i16 + i8 = i56
define i49 @load_i49(ptr %a) {
; CHECK-LABEL: i64 @load_i49
; CHECK: load i32
; CHECK-NEXT: zext {{.*}} to i64
; CHECK: load i16
; CHECK-NEXT: zext {{.*}} to i64
; CHECK: load i8
; CHECK-NEXT: zext {{.*}} to i64
; CHECK: or
entry:
  %1 = load i49, ptr %a
  ret i49 %1
}

; Uses i64
define i57 @load_i57(ptr %a) {
; CHECK-LABEL: @load_i57
; CHECK: load i64
; CHECK-NOT: zext 
entry:
  %1 = load i57, ptr %a
  ret i57 %1
}

; Uses i64
define i64 @load_i64(ptr %a) {
; CHECK-LABEL: @load_i64
; CHECK: load i64
entry:
  %1 = load i64, ptr %a
  ret i64 %1
}

define float @load_float(ptr %a) {
; CHECK-LABEL: define float @load_float(ptr %a)
; CHECK: load float, ptr %a
entry:
  %1 = load float, ptr %a
  ret float %1
}

define double @load_double(ptr %a) {
; CHECK-LABEL: define double @load_double(ptr %a)
; CHECK: load double, ptr %a
entry:
  %1 = load double, ptr %a
  ret double %1
}

define ptr @load_ptr(ptr %a) {
; CHECK-LABEL: define ptr @load_ptr(ptr %a)
; CHECK: load ptr, ptr %a
entry:
  %1 = load ptr, ptr %a
  ret ptr %1
}

