; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

define i8 @fshl_i8(i8 %a, i8 %b, i8 %c) {
; CHECK-LABEL: fshl_i8
; CHECK-NEXT: local.get       0
; CHECK-NEXT: i32.const       8
; CHECK-NEXT: i32.shl
; CHECK-NEXT: local.get       1
; CHECK-NEXT: i32.const       255
; CHECK-NEXT: i32.and
; CHECK-NEXT: i32.or
; CHECK-NEXT: local.get       2
; CHECK-NEXT: i32.const       255
; CHECK-NEXT: i32.and
; CHECK-NEXT: i32.const       8
; CHECK-NEXT: i32.rem_u
; CHECK-NEXT: i32.shl
; CHECK-NEXT: i32.const       8
; CHECK-NEXT: i32.shr_u
    %res = call i8 @llvm.fshl.i8(i8 %a, i8 %b, i8 %c)
    ret i8 %res
}

define i11 @fshl_i11(i11 %a, i11 %b, i11 %c) {
; CHECK-LABEL: fshl_i11
; CHECK-NEXT: local.get       0
; CHECK-NEXT: i32.const       11
; CHECK-NEXT: i32.shl
; CHECK-NEXT: local.get       1
; CHECK-NEXT: i32.const       2047
; CHECK-NEXT: i32.and
; CHECK-NEXT: i32.or
; CHECK-NEXT: local.get       2
; CHECK-NEXT: i32.const       2047
; CHECK-NEXT: i32.and
; CHECK-NEXT: i32.const       11
; CHECK-NEXT: i32.rem_u
; CHECK-NEXT: i32.shl
; CHECK-NEXT: i32.const       11
; CHECK-NEXT: i32.shr_u
    %res = call i11 @llvm.fshl.i11(i11 %a, i11 %b, i11 %c)
    ret i11 %res
}

define i12 @fshl_i12(i12 %a, i12 %b, i12 %c) {
; CHECK-LABEL: fshl_i12
; CHECK-NEXT: local.get       0
; CHECK-NEXT: i32.const       12
; CHECK-NEXT: i32.shl
; CHECK-NEXT: local.get       1
; CHECK-NEXT: i32.const       4095
; CHECK-NEXT: i32.and
; CHECK-NEXT: i32.or
; CHECK-NEXT: local.get       2
; CHECK-NEXT: i32.const       4095
; CHECK-NEXT: i32.and
; CHECK-NEXT: i32.const       12
; CHECK-NEXT: i32.rem_u
; CHECK-NEXT: i32.shl
; CHECK-NEXT: i32.const       12
; CHECK-NEXT: i32.shr_u
    %res = call i12 @llvm.fshl.i12(i12 %a, i12 %b, i12 %c)
    ret i12 %res
}

define i32 @fshl_i32(i32 %a, i32 %b, i32 %c) {
; CHECK-LABEL: fshl_i32
; CHECK-NEXT: local.get       0
; CHECK-NEXT: local.get       2
; CHECK-NEXT: i32.shl
; CHECK-NEXT: local.get       1
; CHECK-NEXT: i32.const       1
; CHECK-NEXT: i32.shr_u
; CHECK-NEXT: local.get       2
; CHECK-NEXT: i32.const       -1
; CHECK-NEXT: i32.xor
; CHECK-NEXT: i32.shr_u
; CHECK-NEXT: i32.or
    %res = call i32 @llvm.fshl.i32(i32 %a, i32 %b, i32 %c)
    ret i32 %res
}

define i64 @fshl_i64(i64 %a, i64 %b, i64 %c) {
; CHECK-LABEL: fshl_i64
; CHECK-NEXT: local.get       0
; CHECK-NEXT: local.get       2
; CHECK-NEXT: i64.shl
; CHECK-NEXT: local.get       1
; CHECK-NEXT: i64.const       1
; CHECK-NEXT: i64.shr_u
; CHECK-NEXT: local.get       2
; CHECK-NEXT: i64.const       -1
; CHECK-NEXT: i64.xor
; CHECK-NEXT: i64.shr_u
; CHECK-NEXT: i64.or
    %res = call i64 @llvm.fshl.i64(i64 %a, i64 %b, i64 %c)
    ret i64 %res
}

define i8 @fshl_i8_rot(i8 %a, i8 %b) {
; CHECK-LABEL: fshl_i8_rot
; CHECK-NEXT: local.get       0
; CHECK-NEXT: local.get       0
; CHECK-NEXT: i32.const       255
; CHECK-NEXT: i32.and
; CHECK-NEXT: i32.const       8
; CHECK-NEXT: i32.rem_u
; CHECK-NEXT: local.tee       1
; CHECK-NEXT: i32.shl
; CHECK-NEXT: local.get       0
; CHECK-NEXT: i32.const       254
; CHECK-NEXT: i32.and
; CHECK-NEXT: i32.const       1
; CHECK-NEXT: i32.shr_u
; CHECK-NEXT: i32.const       7
; CHECK-NEXT: local.get       1
; CHECK-NEXT: i32.sub
; CHECK-NEXT: i32.const       255
; CHECK-NEXT: i32.and
; CHECK-NEXT: i32.shr_u
; CHECK-NEXT: i32.or
    %res = call i8 @llvm.fshl.i8(i8 %a, i8 %a, i8 %b)
    ret i8 %res
}

define i11 @fshl_i11_rot(i11 %a, i11 %b) {
; CHECK-LABEL: fshl_i11_rot
; CHECK-NEXT: local.get       0
; CHECK-NEXT: local.get       0
; CHECK-NEXT: i32.const       2047
; CHECK-NEXT: i32.and
; CHECK-NEXT: i32.const       11
; CHECK-NEXT: i32.rem_u
; CHECK-NEXT: local.tee       1
; CHECK-NEXT: i32.shl
; CHECK-NEXT: local.get       0
; CHECK-NEXT: i32.const       2046
; CHECK-NEXT: i32.and
; CHECK-NEXT: i32.const       1
; CHECK-NEXT: i32.shr_u
; CHECK-NEXT: i32.const       10
; CHECK-NEXT: local.get       1
; CHECK-NEXT: i32.sub
; CHECK-NEXT: i32.const       2047
; CHECK-NEXT: i32.and
; CHECK-NEXT: i32.shr_u
; CHECK-NEXT: i32.or
    %res = call i11 @llvm.fshl.i11(i11 %a, i11 %a, i11 %b)
    ret i11 %res
}

define i12 @fshl_i12_rot(i12 %a, i12 %b) {
; CHECK-LABEL: fshl_i12_rot
; CHECK-NEXT: local.get       0
; CHECK-NEXT: local.get       0
; CHECK-NEXT: i32.const       4095
; CHECK-NEXT: i32.and
; CHECK-NEXT: i32.const       12
; CHECK-NEXT: i32.rem_u
; CHECK-NEXT: local.tee       1
; CHECK-NEXT: i32.shl
; CHECK-NEXT: local.get       0
; CHECK-NEXT: i32.const       4094
; CHECK-NEXT: i32.and
; CHECK-NEXT: i32.const       1
; CHECK-NEXT: i32.shr_u
; CHECK-NEXT: i32.const       11
; CHECK-NEXT: local.get       1
; CHECK-NEXT: i32.sub
; CHECK-NEXT: i32.const       4095
; CHECK-NEXT: i32.and
; CHECK-NEXT: i32.shr_u
; CHECK-NEXT: i32.or
    %res = call i12 @llvm.fshl.i12(i12 %a, i12 %a, i12 %b)
    ret i12 %res
}

define i32 @fshl_i32_rot(i32 %a, i32 %b) {
; CHECK-LABEL: fshl_i32_rot
; CHECK-NEXT: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: i32.rotl
    %res = call i32 @llvm.fshl.i32(i32 %a, i32 %a, i32 %b)
    ret i32 %res
}

define i64 @fshl_i64_rot(i64 %a, i64 %b) {
; CHECK-LABEL: fshl_i64_rot
; CHECK-NEXT: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: i64.rotl
    %res = call i64 @llvm.fshl.i64(i64 %a, i64 %a, i64 %b)
    ret i64 %res
}