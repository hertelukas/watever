; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

define i32 @smax_i32(i32 %a, i32 %b) {
; CHECK-LABEL: smax_i32
; CHECK-NEXT: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: i32.gt_s
; CHECK-NEXT: select
    %res = call i32 @llvm.smax.i32(i32 %a, i32 %b)
    ret i32 %res
}

define i32 @smin_i32(i32 %a, i32 %b) {
; CHECK-LABEL: smin_i32
; CHECK-NEXT: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: i32.lt_s
; CHECK-NEXT: select
    %res = call i32 @llvm.smin.i32(i32 %a, i32 %b)
    ret i32 %res
}

define i32 @umax_i32(i32 %a, i32 %b) {
; CHECK-LABEL: umax_i32
; CHECK-NEXT: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: i32.gt_u
; CHECK-NEXT: select
    %res = call i32 @llvm.umax.i32(i32 %a, i32 %b)
    ret i32 %res
}

define i32 @umin_i32(i32 %a, i32 %b) {
; CHECK-LABEL: umin_i32
; CHECK-NEXT: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: i32.lt_u
; CHECK-NEXT: select
    %res = call i32 @llvm.umin.i32(i32 %a, i32 %b)
    ret i32 %res
}

define i64 @smax_i64(i64 %a, i64 %b) {
; CHECK-LABEL: smax_i64
; CHECK-NEXT: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: i64.gt_s
; CHECK-NEXT: select
    %res = call i64 @llvm.smax.i64(i64 %a, i64 %b)
    ret i64 %res
}

define i64 @smin_i64(i64 %a, i64 %b) {
; CHECK-LABEL: smin_i64
; CHECK-NEXT: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: i64.lt_s
; CHECK-NEXT: select
    %res = call i64 @llvm.smin.i64(i64 %a, i64 %b)
    ret i64 %res
}

define i64 @umax_i64(i64 %a, i64 %b) {
; CHECK-LABEL: umax_i64
; CHECK-NEXT: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: i64.gt_u
; CHECK-NEXT: select
    %res = call i64 @llvm.umax.i64(i64 %a, i64 %b)
    ret i64 %res
}

define i64 @umin_i64(i64 %a, i64 %b) {
; CHECK-LABEL: umin_i64
; CHECK-NEXT: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: i64.lt_u
; CHECK-NEXT: select
    %res = call i64 @llvm.umin.i64(i64 %a, i64 %b)
    ret i64 %res
}

define i8 @smax_i8(i8 %a, i8 %b) {
; CHECK-LABEL: smax_i8
; CHECK: i32.extend8_s
; CHECK: i32.extend8_s
; CHECK: i32.gt_s
; CHECK-NEXT: select
    %res = call i8 @llvm.smax.i8(i8 %a, i8 %b)
    ret i8 %res
}

define i8 @smin_i8(i8 %a, i8 %b) {
; CHECK-LABEL: smin_i8
; CHECK: i32.extend8_s
; CHECK: i32.extend8_s
; CHECK: i32.lt_s
; CHECK-NEXT: select
    %res = call i8 @llvm.smin.i8(i8 %a, i8 %b)
    ret i8 %res
}

define i8 @umax_i8(i8 %a, i8 %b) {
; CHECK-LABEL: umax_i8
; CHECK: i32.const 255
; CHECK-NEXT: i32.and
; CHECK: i32.const 255
; CHECK-NEXT: i32.and
; CHECK: i32.gt_u
; CHECK-NEXT: select
    %res = call i8 @llvm.umax.i8(i8 %a, i8 %b)
    ret i8 %res
}

define i8 @umin_i8(i8 %a, i8 %b) {
; CHECK-LABEL: umin_i8
; CHECK: i32.const 255
; CHECK-NEXT: i32.and
; CHECK: i32.const 255
; CHECK-NEXT: i32.and
; CHECK: i32.lt_u
; CHECK-NEXT: select
    %res = call i8 @llvm.umin.i8(i8 %a, i8 %b)
    ret i8 %res
}