; RUN: watever -l 0 --legal %s | FileCheck %s

define i32 @select_i32(i1 %cond) {
; CHECK-LABEL: select_i32
; CHECK: %[[AND:[0-9]+]] = and i32 %cond, 1
; CHECK-NEXT: %[[COND:[0-9]+]] = trunc i32 %[[AND]] to i1
; CHECK-NEXT: select i1 %[[COND]], i32 17, i32 42
entry:
  %1 = select i1 %cond, i32 17, i32 42
  ret i32 %1
}

define i128 @select_i128(i1 %cond, i128 %a, i128 %b) {
; CHECK-LABEL: select_i128
; CHECK: %[[AND:[0-9]+]] = and i32 %cond, 1
; CHECK-NEXT: %[[COND:[0-9]+]] = trunc i32 %[[AND]] to i1
; CHECK: select i1 %[[COND]]
; CHECK: select i1 %[[COND]]
entry:
  %1 = select i1 %cond, i128 %a, i128 %b
  ret i128 %1
}