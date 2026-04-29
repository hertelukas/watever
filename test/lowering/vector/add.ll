; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

define <4 x i32> @foo(<4 x i32> %a, <4 x i32> %b) {
; CHECK-LABEL: foo
; CHECK-NEXT: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: i32x4.add
    %sum = add <4 x i32> %a, %b
    ret <4 x i32> %sum
}
