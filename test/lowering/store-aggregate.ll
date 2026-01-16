; RUN: watever -l 0 --legal %s | FileCheck %s

%MyStruct = type { i32, float, [4 x i8] }

@struct_instance = global %MyStruct zeroinitializer

define void @store_struct() {
; CHECK-LABEL: @store_struct
; CHECK:      %ptr = alloca %MyStruct, align 8

; CHECK-NEXT: store i32 0, ptr %ptr, align 4

; CHECK-NEXT: ptrtoint ptr %ptr to i64
; CHECK-NEXT: add i64 %1, 4
; CHECK-NEXT: inttoptr i64 %2 to ptr
; CHECK-NEXT: store float 0.000000e+00, ptr {{.*}}, align 4

; CHECK-NEXT: ptrtoint ptr %ptr to i64
; CHECK-NEXT: add i64 %{{.*}}, 8
; CHECK-NEXT: inttoptr i64 {{.*}} to ptr
; CHECK-NEXT: store i8 0, ptr {{.*}}, align 1
; CHECK-NEXT: ptrtoint ptr %ptr to i64
; CHECK-NEXT: add i64 {{.*}}, 9
; CHECK-NEXT: inttoptr i64 {{.*}} to ptr
; CHECK-NEXT: store i8 0, ptr {{.*}}, align 1
; CHECK-NEXT: ptrtoint ptr %ptr to i64
; CHECK-NEXT: add i64 {{.*}}, 10
; CHECK-NEXT: inttoptr i64 {{.*}} to ptr
; CHECK-NEXT: store i8 0, ptr {{.*}}, align 1
; CHECK-NEXT: ptrtoint ptr %ptr to i64
; CHECK-NEXT: add i64 {{.*}}, 11
; CHECK-NEXT: inttoptr i64 {{.*}} to ptr
; CHECK-NEXT: store i8 0, ptr {{.*}}, align 1 
  %ptr = alloca %MyStruct
  store %MyStruct zeroinitializer, %MyStruct* %ptr
  ret void
}
