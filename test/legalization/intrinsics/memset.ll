; RUN: watever -l 0 --legal %s | FileCheck %s

declare void @llvm.memset.p0.i32(ptr writeonly captures(none), i8, i32, i1 immarg)

define void @memset_i32(ptr %dest, i8 %val, i32 %len) {
; CHECK-LABEL: @memset_i32(ptr %dest, i32 %val, i32 %len)
; CHECK: trunc i32 %val to i8
; CHECK: call void @llvm.memset.p0.i32
  call void @llvm.memset.p0.i32(ptr %dest, i8 %val, i32 %len, i1 false)
  ret void
}
