; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

declare i2 @llvm.scmp.i2.i32(i32, i32)
declare i8 @llvm.ucmp.i8.i32(i32, i32)
declare i32 @llvm.scmp.i32.i64(i64, i64)
declare i64 @llvm.ucmp.i64.i64(i64, i64)

define i2 @i2_scmp_i32(i32 %a, i32 %b) {
; CHECK-LABEL: $i2_scmp_i32
; CHECK-NEXT: local.get       0
; CHECK-NEXT: local.get       1
; CHECK-NEXT: i32.gt_s
; CHECK-NEXT: local.get       0
; CHECK-NEXT: local.get       1
; CHECK-NEXT: i32.lt_s
; CHECK-NEXT: i32.sub 
    %res = call i2 @llvm.scmp.i2.i32(i32 %a, i32 %b)
    ret i2 %res
}

define i8 @i8_ucmp_i32(i32 %a, i32 %b) {
; CHECK-LABEL: $i8_ucmp_i32
; CHECK-NEXT: local.get       0
; CHECK-NEXT: local.get       1
; CHECK-NEXT: i32.gt_u
; CHECK-NEXT: local.get       0
; CHECK-NEXT: local.get       1
; CHECK-NEXT: i32.lt_u
; CHECK-NEXT: i32.sub 
    %res = call i8 @llvm.ucmp.i8.i32(i32 %a, i32 %b)
    ret i8 %res
}

define i32 @i32_scmp_i64(i64 %a, i64 %b) {
; CHECK-LABEL: $i32_scmp_i64
; CHECK-NEXT: local.get       0
; CHECK-NEXT: local.get       1
; CHECK-NEXT: i64.gt_s
; CHECK-NEXT: local.get       0
; CHECK-NEXT: local.get       1
; CHECK-NEXT: i64.lt_s
; CHECK-NEXT: i32.sub 
    %res = call i32 @llvm.scmp.i32.i64(i64 %a, i64 %b)
    ret i32 %res
}

define i64 @i64_ucmp_i64(i64 %a, i64 %b) {
; CHECK-LABEL: $i64_ucmp_i64
; CHECK-NEXT: local.get       0
; CHECK-NEXT: local.get       1
; CHECK-NEXT: i64.gt_u
; CHECK-NEXT: local.get       0
; CHECK-NEXT: local.get       1
; CHECK-NEXT: i64.lt_u
; CHECK-NEXT: i32.sub
; CHECK-NEXT: i64.extend_i32_s
    %res = call i64 @llvm.ucmp.i64.i64(i64 %a, i64 %b)
    ret i64 %res
}

