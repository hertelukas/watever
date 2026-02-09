; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

declare i8 @llvm.abs.i8(i8 , i1)
declare i15 @llvm.abs.i15(i15 , i1)
declare i32 @llvm.abs.i32(i32 , i1)

define i8 @abs_i8(i8 %a) {
; CHECK-LABEL: $abs_i8
; CHECK:      local.get       0
; CHECK-NEXT: local.get       0
; CHECK-NEXT: i32.extend8_s
; CHECK-NEXT: i32.const      31
; CHECK-NEXT: i32.shr_s
; CHECK-NEXT: local.tee       1
; CHECK-NEXT: i32.xor 
; CHECK-NEXT: local.get       1
; CHECK-NEXT: i32.sub 
  %1 = call i8 @llvm.abs.i8(i8 %a, i1 true)
  ret i8 %1
}

define i15 @abs_i15(i15 %a) {
; CHECK-LABEL: $abs_i15
; CHECK:      local.get       0
; CHECK-NEXT: local.get       0
; CHECK-NEXT: i32.const      17
; CHECK-NEXT: i32.shl
; CHECK-NEXT: i32.const      17
; CHECK-NEXT: i32.shr_s
; CHECK-NEXT: i32.const      31
; CHECK-NEXT: i32.shr_s
; CHECK-NEXT: local.tee       1
; CHECK-NEXT: i32.xor 
; CHECK-NEXT: local.get       1
; CHECK-NEXT: i32.sub 
  %1 = call i15 @llvm.abs.i15(i15 %a, i1 true)
  ret i15 %1
}

define i32 @abs_i32(i32 %a) {
; CHECK-LABEL: $abs_i32
; CHECK:      local.get       0
; CHECK-NEXT: local.get       0
; CHECK-NEXT: i32.const       31
; CHECK-NEXT: i32.shr_s
; CHECK-NEXT: local.tee       1
; CHECK-NEXT: i32.xor 
; CHECK-NEXT: local.get       1
; CHECK-NEXT: i32.sub 
  %1 = call i32 @llvm.abs.i32(i32 %a, i1 true)
  ret i32 %1
}
