; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

define void @foo() {
; CHECK-LABEL: $foo
  switch i32 0, label %exit []
exit:
  ret void
}