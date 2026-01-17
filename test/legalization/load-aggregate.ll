; RUN: watever -l 0 --legal %s | FileCheck %s

%MyStruct = type { i32, float, [4 x i8] }

@struct_instance = global %MyStruct zeroinitializer

define void @load_struct() {
; CHECK-LABEL: @load_struct
; CHECK:  %ptr = alloca %MyStruct, align 8

; Load the i32
; CHECK-NEXT: %1 = load i32, ptr @struct_instance, align 4

; Load the float
; CHECK-NEXT: %2 = ptrtoint ptr @struct_instance to i64
; CHECK-NEXT: %3 = add i64 %2, 4
; CHECK-NEXT: %4 = inttoptr i64 %3 to ptr
; CHECK-NEXT: %5 = load float, ptr %4, align 4

; Load the i8s
; CHECK-NEXT: %6 = ptrtoint ptr @struct_instance to i64
; CHECK-NEXT: %7 = add i64 %6, 8
; CHECK-NEXT: %8 = inttoptr i64 %7 to ptr
; CHECK-NEXT: %9 = load i8, ptr %8, align 4
; CHECK-NEXT: %10 = zext i8 %9 to i32
; CHECK-NEXT: %11 = ptrtoint ptr @struct_instance to i64
; CHECK-NEXT: %12 = add i64 %11, 9
; CHECK-NEXT: %13 = inttoptr i64 %12 to ptr
; CHECK-NEXT: %14 = load i8, ptr %13, align 1
; CHECK-NEXT: %15 = zext i8 %14 to i32
; CHECK-NEXT: %16 = ptrtoint ptr @struct_instance to i64
; CHECK-NEXT: %17 = add i64 %16, 10
; CHECK-NEXT: %18 = inttoptr i64 %17 to ptr
; CHECK-NEXT: %19 = load i8, ptr %18, align 2
; CHECK-NEXT: %20 = zext i8 %19 to i32
; CHECK-NEXT: %21 = ptrtoint ptr @struct_instance to i64
; CHECK-NEXT: %22 = add i64 %21, 11
; CHECK-NEXT: %23 = inttoptr i64 %22 to ptr
; CHECK-NEXT: %24 = load i8, ptr %23, align 1
; CHECK-NEXT: %25 = zext i8 %24 to i32
  %ptr = alloca %MyStruct
  %1 = load %MyStruct, ptr @struct_instance
  store %MyStruct %1, %MyStruct* %ptr
  ret void
}
