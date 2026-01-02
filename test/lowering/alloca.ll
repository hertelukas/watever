; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-unknown"

; TODO write actual tests instead of only checking if it compiles

declare void @sink(ptr, ptr)

define void @alloca_i32() {
; CHECK-LABEL: alloca_i32
; CHECK: global.get
; CHECK: i32.const 16
; CHECK: i32.sub
    %1 = alloca i32
    store i32 3, ptr %1
    ret void
}

define void @alloca_i32_i64() {
    %1 = alloca i32
    %2 = alloca i64
    store i32 3, ptr %1
    store i64 4, ptr %2
    ret void
}

define void @alloca_i32_i64_i64() {
    %1 = alloca i32
    %2 = alloca i64
    %3 = alloca i64
    store i32 3, ptr %1
    store i64 4, ptr %2
    store i64 5, ptr %3
    ret void
}

define void @alloca_i32_dynamic(i32 %n) {
  %static = alloca i32
  %dynamic = alloca i8, i32 %n
  
  call void @sink(ptr %static, ptr %dynamic)
  ret void
}

define void @alloca_dynamic_static(i32 %n) {
  %dynamic = alloca i8, i32 %n
  %static = alloca i32
  
  call void @sink(ptr %static, ptr %dynamic)
  ret void
}

define void @alloca_conditional(i1 %cond) {
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