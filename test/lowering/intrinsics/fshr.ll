; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

define i32 @fshr_i32(i32 %a, i32 %b, i32 %c) {
; CHECK-LABEL: fshr_i32
; CHECK-NEXT: local.get       0
; CHECK-NEXT: i32.const       1
; CHECK-NEXT: i32.shl
; CHECK-NEXT: local.get       2
; CHECK-NEXT: i32.const       -1
; CHECK-NEXT: i32.xor
; CHECK-NEXT: i32.shl
; CHECK-NEXT: local.get       1
; CHECK-NEXT: local.get       2
; CHECK-NEXT: i32.shr_u
; CHECK-NEXT: i32.or
    %res = call i32 @llvm.fshr.i32(i32 %a, i32 %b, i32 %c)
    ret i32 %res
}

define i64 @fshr_i64(i64 %a, i64 %b, i64 %c) {
; CHECK-LABEL: fshr_i64
; CHECK-NEXT: local.get       0
; CHECK-NEXT: i64.const       1
; CHECK-NEXT: i64.shl
; CHECK-NEXT: local.get       2
; CHECK-NEXT: i64.const       -1
; CHECK-NEXT: i64.xor
; CHECK-NEXT: i64.shl
; CHECK-NEXT: local.get       1
; CHECK-NEXT: local.get       2
; CHECK-NEXT: i64.shr_u
; CHECK-NEXT: i64.or
    %res = call i64 @llvm.fshr.i64(i64 %a, i64 %b, i64 %c)
    ret i64 %res
}

define i32 @fshr_i32_rot(i32 %a, i32 %b) {
; CHECK-LABEL: fshr_i32_rot
; CHECK-NEXT: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: i32.rotr
    %res = call i32 @llvm.fshr.i32(i32 %a, i32 %a, i32 %b)
    ret i32 %res
}

define i64 @fshr_i64_rot(i64 %a, i64 %b) {
; CHECK-LABEL: fshr_i64_rot
; CHECK-NEXT: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: i64.rotr
    %res = call i64 @llvm.fshr.i64(i64 %a, i64 %a, i64 %b)
    ret i64 %res
}