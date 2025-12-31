; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

define void @unreachable(i32 %a) {
; CHECK-LABEL: unreachable
; CHECK: unreachable
 unreachable
}

