; RUN: watever -l 0 --legal %s | FileCheck %s

define i32 @main() {
; CHECK-LABEL: @main
; CHECK ret i32 0
  ret i32 0
}