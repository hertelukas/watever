; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-unknown"

%MyStruct = type { i32, float, [4 x i8] }

@struct_instance = global %MyStruct zeroinitializer

define void @load_struct() {
; CHECK-LABEL: $load_struct
  %ptr = alloca %MyStruct
  %1 = load %MyStruct, ptr @struct_instance
  store %MyStruct %1, %MyStruct* %ptr
  ret void
}
