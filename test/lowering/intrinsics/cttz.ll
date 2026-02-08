; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

declare i32 @llvm.cttz.i32(i32)

define i32 @cttz_i32(i32 %val) {
; CHECK-LABEL: $cttz_i32
; CHECK-NEXT: local.get 0
; CHECK-NEXT: i32.ctz
  %1 = call i32 @llvm.cttz.i32(i32 %val)
  ret i32 %1
}
