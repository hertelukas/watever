; RUN: watever %s -o %t.o | wasm-objdump -x %t.o | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-unknown"

@value = hidden global i32 42, align 4
@pointer = hidden global ptr @value, align 4
@pointer_pair = hidden global { ptr, ptr} { ptr @value, ptr @pointer}, align 4
@function_pointer = global ptr @function, align 4

define void @function() {
  ret void
}

; CHECK: Data[4]
; CHECK-NEXT:  segment[0] <.data.value> memory=0 size=4 - init i32=0
; CHECK-NEXT:  2a00 0000
; CHECK: segment[1] <.data.pointer> memory=0 size=4 - init i32=4
; CHECK: segment[2] <.data.pointer_pair> memory=0 size=8 - init i32=8
; CHECK: segment[3] <.data.function_pointer> memory=0 size=4 - init i32=16
; CHECK: Custom:
; CHECK-NEXT:  - name: "linking"
; CHECK-NEXT:   - symbol table
; CHECK: Custom:
; CHECK-NEXT:  - name: "reloc.DATA"
; CHECK-NEXT:   - relocations for section: {{.}} (Data) [4]
; CHECK-NEXT:    - R_WASM_MEMORY_ADDR_I32 offset=0x00000f(file={{.*}}) symbol={{.}} <value>
; CHECK-NEXT:    - R_WASM_MEMORY_ADDR_I32 offset=0x000018(file={{.*}}) symbol={{.}} <value>
; CHECK-NEXT:    - R_WASM_MEMORY_ADDR_I32 offset=0x00001c(file={{.*}}) symbol={{.}} <pointer>
; CHECK-NEXT:    - R_WASM_TABLE_INDEX_I32 offset=0x000025(file={{.*}}) symbol={{.}} <function>