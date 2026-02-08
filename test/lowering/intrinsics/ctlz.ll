; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

declare i32 @llvm.ctlz.i32(i32)

define i32 @ctlz_i32(i32 %val) {
; CHECK-LABEL: $ctlz_i32
; CHECK-NEXT: local.get 0
; CHECK-NEXT: i32.clz
  %1 = call i32 @llvm.ctlz.i32(i32 %val)
  ret i32 %1
}
