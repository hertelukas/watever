; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-unknown"

define i32 @i32_1(ptr %foo) {
; CHECK-LABEL: $i32_1
; CHECK: i32.load align=1
    %1 = load i32, ptr %foo, align 1
    ret i32 %1
}

define i32 @i32_2(ptr %foo) {
; CHECK-LABEL: $i32_2
; CHECK: i32.load align=2
    %1 = load i32, ptr %foo, align 2
    ret i32 %1
}

define i32 @i32_4(ptr %foo) {
; CHECK-LABEL: $i32_4
; CHECK: i32.load
; CHECK-NOT: align
    %1 = load i32, ptr %foo, align 4
    ret i32 %1
}

define i64 @i64_1(ptr %foo) {
; CHECK-LABEL: $i64_1
; CHECK: i64.load align=1
    %1 = load i64, ptr %foo, align 1
    ret i64 %1
}

define i64 @i64_2(ptr %foo) {
; CHECK-LABEL: $i64_2
; CHECK: i64.load align=2
    %1 = load i64, ptr %foo, align 2
    ret i64 %1
}

define i64 @i64_4(ptr %foo) {
; CHECK-LABEL: $i64_4
; CHECK: i64.load align=4
    %1 = load i64, ptr %foo, align 4
    ret i64 %1
}

define i64 @i64_8(ptr %foo) {
; CHECK-LABEL: $i64_8
; CHECK: i64.load
; CHECK-NOT: align
    %1 = load i64, ptr %foo, align 8
    ret i64 %1
}

%MyStruct = type { i32, float, [4 x i8] }

@struct_instance = global %MyStruct zeroinitializer

define %MyStruct @load_struct() {
; CHECK-LABEL: $load_struct
  %1 = load %MyStruct, ptr @struct_instance
  ret %MyStruct %1
}
