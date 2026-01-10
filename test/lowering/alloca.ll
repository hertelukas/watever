; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-unknown"

; The static stack frame is sorted by descending size, in order to minimize padding
; Example Layout for: %1 = alloca i32, %2 = alloca i64
;
; Offset | Content                | Variable
; -------+------------------------+---------
; 8      | [ 8 bytes (i64)      ] | %2
; 4      | [ 4 bytes (i32)      ] | %1
; 0      | [ 4 bytes (Padding)  ] |
;
; Visualized:
; +----------------------+ <--- SP + 16 (Previous Stack Pointer)
; |                      |
; |     %2 (i64)         |  Offset: 8
; |                      |
; +----------------------+
; |     %1 (i32)         |  Offset: 4
; +----------------------+
; |     Padding          |  Offset: 0
; +----------------------+ <--- FP

declare void @sink(ptr, ptr)

define void @alloca_i32() {
; CHECK-LABEL: alloca_i32
;
; Increase SP
; CHECK: global.get
; CHECK-NEXT: i32.const 16
; CHECK-NEXT: i32.sub
;
; Set FP & SP
; CHECK-NEXT: local.tee [[FP:[0-9]+]]
; CHECK-NEXT: global.set [[SP:[0-9]+]]
;
; Do store
; CHECK: local.get [[FP]]
; CHECK-NEXT: i32.const 3
; CHECK-NEXT: i32.store offset=12
;
; Restore SP
; CHECK: local.get [[FP]]
; CHECK-NEXT: i32.const 16
; CHECK-NEXT: i32.add
; CHECK-NEXT: global.set [[SP]]
    %1 = alloca i32
    store i32 3, ptr %1
    ret void
}

define void @alloca_i32_i64() {
; CHECK-LABEL: alloca_i32_i64
; CHECK: i32.const 3
; CHECK-NEXT: i32.store offset=4
; CHECK: i64.const 4
; CHECK-NEXT: i64.store offset=8
    %1 = alloca i32
    %2 = alloca i64
    store i32 3, ptr %1
    store i64 4, ptr %2
    ret void
}

define void @alloca_i32_i64_i64() {
; CHECK-LABEL: alloca_i32_i64_i64
;
; Increase SP
; CHECK: global.get
; CHECK-NEXT: i32.const 32
; CHECK-NEXT: i32.sub
;
; Set FP & SP
; CHECK-NEXT: local.tee [[FP:[0-9]+]]
; CHECK-NEXT: global.set [[SP:[0-9]+]]
;
; Do stores
; CHECK: local.get [[FP]]
; CHECK-NEXT: i32.const 3
; CHECK-NEXT: i32.store offset=12
; CHECK: local.get [[FP]]
; CHECK-NEXT: i64.const 4
; CHECK-NEXT: i64.store offset=24
; CHECK: local.get [[FP]]
; CHECK-NEXT: i64.const 5
; CHECK-NEXT: i64.store offset=16
;
; Restore SP
; CHECK: local.get [[FP]]
; CHECK-NEXT: i32.const 32
; CHECK-NEXT: i32.add
; CHECK-NEXT: global.set [[SP]]
    %1 = alloca i32
    %2 = alloca i64
    %3 = alloca i64
    store i32 3, ptr %1
    store i64 4, ptr %2
    store i64 5, ptr %3
    ret void
}

define void @alloca_i32_dynamic(i32 %n) {
; CHECK-LABEL: alloca_i32_dynamic
; Increase SP
; CHECK: global.get
; CHECK-NEXT: i32.const 16
; CHECK-NEXT: i32.sub
;
; Set FP & SP
; CHECK-NEXT: local.tee [[FP:[0-9]+]]
; CHECK-NEXT: global.set [[SP:[0-9]+]]
;
; Materialize first argument on the stack
; CHECK: local.get [[FP]]
; CHECK-NEXT: i32.const 12
; CHECK-NEXT: i32.add
;
; Calculate size of dynamic
; CHECK: local.get 0
; Size of i8
; CHECK-NEXT: i32.const 1
; CHECK-NEXT: i32.mul
; Align to 16-byte
; CHECK-NEXT: i32.const 15
; CHECK-NEXT: i32.add
; CHECK-NEXT: i32.const -16
; CHECK-NEXT: i32.and
; CHECK-NEXT: local.set [[DYN_SIZE:[0-9]+]]
;
; Update SP (allocate dynamic)
; CHECK-NEXT: global.get [[SP]]
; CHECK-NEXT: local.get [[DYN_SIZE]]
; CHECK-NEXT: i32.sub
; CHECK-NEXT: global.set [[SP]]
;
; Materialize second argument on the stack and call
; CHECK-NEXT: global.get [[SP]]
; CHECK-NEXT: call
;
; Restore SP
; CHECK: local.get [[FP]]
; CHECK-NEXT: i32.const 16
; CHECK-NEXT: i32.add
; CHECK-NEXT: global.set [[SP]]
  %static = alloca i32
  %dynamic = alloca i8, i32 %n
  
  call void @sink(ptr %static, ptr %dynamic)
  ret void
}

define void @alloca_dynamic_i32(i32 %n) {
; CHECK-LABEL: alloca_dynamic_i32
; This should be the same as alloca_i32_dynamic
; Increase SP
; CHECK: global.get
; CHECK-NEXT: i32.const 16
; CHECK-NEXT: i32.sub
;
; Set FP & SP
; CHECK-NEXT: local.tee [[FP:[0-9]+]]
; CHECK-NEXT: global.set [[SP:[0-9]+]]
;
; Materialize first argument on the stack
; CHECK: local.get [[FP]]
; CHECK-NEXT: i32.const 12
; CHECK-NEXT: i32.add
;
; Calculate size of dynamic
; CHECK: local.get 0
; Size of i8
; CHECK-NEXT: i32.const 1
; CHECK-NEXT: i32.mul
; Align to 16-byte
; CHECK-NEXT: i32.const 15
; CHECK-NEXT: i32.add
; CHECK-NEXT: i32.const -16
; CHECK-NEXT: i32.and
; CHECK-NEXT: local.set [[DYN_SIZE:[0-9]+]]
;
; Update SP (allocate dynamic)
; CHECK-NEXT: global.get [[SP]]
; CHECK-NEXT: local.get [[DYN_SIZE]]
; CHECK-NEXT: i32.sub
; CHECK-NEXT: global.set [[SP]]
;
; Materialize second argument on the stack and call
; CHECK-NEXT: global.get [[SP]]
; CHECK-NEXT: call
;
; Restore SP
; CHECK: local.get [[FP]]
; CHECK-NEXT: i32.const 16
; CHECK-NEXT: i32.add
; CHECK-NEXT: global.set [[SP]]
  %dynamic = alloca i8, i32 %n
  %static = alloca i32
  
  call void @sink(ptr %static, ptr %dynamic)
  ret void
}

define void @alloca_conditional(i1 %cond) {
; CHECK-LABEL: alloca_conditional
; Increase SP
; CHECK: global.get
; CHECK-NEXT: i32.const 16
; CHECK-NEXT: i32.sub
;
; Set FP & SP
; CHECK-NEXT: local.tee [[FP:[0-9]+]]
; CHECK-NEXT: global.set [[SP:[0-9]+]]
;
; Do not allocate anything else statically
; CHECK-NEXT: block
;
; In the if case, do dynamic allocation
; CHECK: global.get [[SP]]
; CHECK-NEXT: i32.const 16
; CHECK-NEXT: i32.sub
; Set SP
; CHECK-NEXT: global.set [[SP]]
; Get second argument
; CHECK-NEXT: global.get [[SP]]
; Note th missing add 12 here, dynamic is at the bottom of the stack
; CHECK-NEXT: call
entry:
  %static = alloca i32
  br i1 %cond, label %then, label %merge

then:
  %dynamic = alloca i32
  call void @sink(ptr %static, ptr %dynamic)
  br label %merge

merge:
  ret void
}
