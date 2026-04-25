; RUN: watever %s -o %t.o
; RUN: wasm-objdump -xd %t.o | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-unknown"

; CHECK: Type[1]:
; CHECK-NEXT: - type[0] () -> nil

; CHECK: Import
; CHECK: - func[0] sig=0 <env.foo> <- env.foo

; CHECK: Code Disassembly:
define void @bar() {
; CHECK-LABEL: func[1] <bar>:
; CHECK: call 0 <env.foo>
  call void @foo()
  ret void
}

declare void @foo(...) "no-prototype"

