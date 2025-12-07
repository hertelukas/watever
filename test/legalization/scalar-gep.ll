; RUN: watever -l 0 --legal %s | FileCheck %s

; struct RT {
;   char A;
;   int B[10][20];
;   char C;
; };
; struct ST {
;   int X;
;   double Y;
;   struct RT Z;
; };

%struct.RT = type { i8, [10 x [20 x i32]], i8 }
%struct.ST = type { i32, double, %struct.RT }

define ptr @zero_offset(ptr %s) {
; CHECK-LABEL: @zero_offset
; CHECK-NEXT: entry 
; CHECK-NEXT: ret ptr %s
entry:
  %idx = getelementptr %struct.ST, ptr %s, i64 0
  ret ptr %idx
}

define ptr @constant_offset(ptr %s) {
; CHECK-LABEL: @constant_offset
; CHECK: add {{.*}}, 1296
entry:
  %arrayidx = getelementptr inbounds %struct.ST, ptr %s, i64 1, i32 2, i32 1, i64 5, i64 13
  ret ptr %arrayidx
}

define ptr @variable_offset(ptr %s, i64 %st.idx, i64 %b.idx) {
; CHECK-LABEL: @variable_offset
; CHECK: mul i64 %st.idx, 824
; CHECK: mul i64 %b.idx, 80
entry:
  %arrayidx = getelementptr inbounds %struct.ST, ptr %s, i64 %st.idx, i32 2, i32 1, i64 %b.idx, i64 13
  ret ptr %arrayidx
}