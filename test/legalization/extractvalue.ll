; RUN: watever -l 0 --legal %s | FileCheck %s

%MyStruct = type { i32, float, [4 x i8] }

@struct_instance = global %MyStruct zeroinitializer

define i8 @extract_scalar() {
; CHECK-LABEL: @extract_scalar
; CHECK: [[PTR_INT_OFFSET:%[0-9]+]] = add i64 {{.*}}, 9
; CHECK: [[PTR_OFFSET:%[0-9]+]] = inttoptr i64 [[PTR_INT_OFFSET]] to ptr
; CHECK: [[RESULT_I8:%[0-9]+]] = load i8, ptr [[PTR_OFFSET]]
; CHECK: [[RESULT:%[0-9]+]] = zext i8 [[RESULT_I8]] to i32
; CHECK: ret i32 [[RESULT]]
  %1 = load %MyStruct, ptr @struct_instance
  %2 = extractvalue %MyStruct %1, 2, 1
  ret i8 %2
}

define void @extract_array() {
; CHECK-LABEL: @extract_array
;
; First byte
; CHECK: [[INT_FIRST:%[0-9]+]] = add i64 {{.*}}, 8
; CHECK: [[PTR_FIRST:%[0-9]+]] = inttoptr i64 [[INT_FIRST]] to ptr
; CHECK: [[I8_FIRST:%[0-9]+]] = load i8, ptr [[PTR_FIRST]]
; CHECK: [[FIRST:%[0-9]+]] = zext i8 [[I8_FIRST]] to i32
; ...
; Last byte
; CHECK: [[INT_LAST:%[0-9]+]] = add i64 {{.*}}, 11
; CHECK: [[PTR_LAST:%[0-9]+]] = inttoptr i64 [[INT_LAST]] to ptr
; CHECK: [[I8_LAST:%[0-9]+]] = load i8, ptr [[PTR_LAST]]
; CHECK: [[LAST:%[0-9]+]] = zext i8 [[I8_LAST]] to i32
;
; CHECK-NOT: store
; Store first byte
; CHECK: [[FIRST_TRUNC:%[0-9]+]] = trunc i32 [[FIRST]] to i8
; CHECK: store i8 [[FIRST_TRUNC]], ptr %ptr
; ...
; CHECK: store
; CHECK: store
; ...
; CHECK: add i64 {{.*}}, 3
; CHECK: [[LAST_TRUNC:%[0-9]+]] = trunc i32 [[LAST]] to i8
; CHECK: store i8 [[LAST_TRUNC]], ptr {{.*}}
  %ptr = alloca [4 x i8]
  %1 = load %MyStruct, ptr @struct_instance
  %2 = extractvalue %MyStruct %1, 2
  store [4 x i8] %2, ptr %ptr
  ret void
}