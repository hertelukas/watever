; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

define i16 @usub_i16(i16 %a, i16 %b) {
; CHECK-LABEL: usub_i16
; CHECK:      local.get 0
; CHECK-NEXT: local.get 0
; CHECK-NEXT: i32.const 65535
; CHECK-NEXT: i32.and
; CHECK-NEXT: local.get 1
; CHECK-NEXT: i32.const 65535
; CHECK-NEXT: i32.and
; CHECK-NEXT: i32.sub
; CHECK-NEXT: local.tee [[VAR:[0-9]+]]
; CHECK-NEXT: local.get [[VAR]]
; CHECK-NEXT: local.get [[VAR]]
; CHECK-NEXT: i32.const 65535
; CHECK-NEXT: i32.and
; CHECK-NEXT: i32.ne
; CHECK:      select
  %1 = tail call { i16, i1 } @llvm.usub.with.overflow.i16(i16 %a, i16 %b)
  %2 = extractvalue { i16, i1 } %1, 0
  %3 = extractvalue { i16, i1 } %1, 1
  %4 = select i1 %3, i16 %a, i16 %2
  ret i16 %4
}

define i16 @ssub_i16(i16 %a, i16 %b) {
; CHECK-LABEL: ssub_i16
; CHECK: local.get 0
; CHECK-NEXT: local.get 0
; CHECK-NEXT: i32.extend16_s
; CHECK-NEXT: local.get 1
; CHECK-NEXT: i32.extend16_s
; CHECK-NEXT: i32.sub
; CHECK-NEXT: local.tee [[VAR:[0-9]+]]
; CHECK-NEXT: local.get [[VAR]]
; CHECK-NEXT: local.get [[VAR]]
; CHECK-NEXT: i32.extend16_s
; CHECK-NEXT: i32.ne
; CHECK:      select
  %1 = tail call { i16, i1 } @llvm.ssub.with.overflow.i16(i16 %a, i16 %b)
  %2 = extractvalue { i16, i1 } %1, 0
  %3 = extractvalue { i16, i1 } %1, 1
  %4 = select i1 %3, i16 %a, i16 %2
  ret i16 %4
}

define i31 @usub_i31(i31 %a, i31 %b) {
; CHECK-LABEL: usub_i31
; CHECK: local.get 0
; CHECK-NEXT: local.get 0
; CHECK-NEXT: i32.const 2147483647
; CHECK-NEXT: i32.and
; CHECK-NEXT: local.get 1
; CHECK-NEXT: i32.const 2147483647
; CHECK-NEXT: i32.and
; CHECK-NEXT: i32.sub
; CHECK-NEXT: local.tee [[VAR:[0-9]+]]
; CHECK-NEXT: local.get [[VAR]]
; CHECK-NEXT: local.get [[VAR]]
; CHECK-NEXT: i32.const 2147483647
; CHECK-NEXT: i32.and
; CHECK-NEXT: i32.ne
; CHECK:      select
  %1 = tail call { i31, i1 } @llvm.usub.with.overflow.i31(i31 %a, i31 %b)
  %2 = extractvalue { i31, i1 } %1, 0
  %3 = extractvalue { i31, i1 } %1, 1
  %4 = select i1 %3, i31 %a, i31 %2
  ret i31 %4
}

define i31 @ssub_i31(i31 %a, i31 %b) {
; CHECK-LABEL: ssub_i31
; CHECK: local.get 0
; CHECK-NEXT: local.get 0
; CHECK-NEXT: i32.const 1
; CHECK-NEXT: i32.shl
; CHECK-NEXT: i32.const 1
; CHECK-NEXT: i32.shr_s
; CHECK-NEXT: local.get 1
; CHECK-NEXT: i32.const 1
; CHECK-NEXT: i32.shl
; CHECK-NEXT: i32.const 1
; CHECK-NEXT: i32.shr_s
; CHECK-NEXT: i32.sub
; CHECK-NEXT: local.tee [[VAR:[0-9]+]]
; CHECK-NEXT: local.get [[VAR]]
; CHECK-NEXT: local.get [[VAR]]
; CHECK-NEXT: i32.const 1
; CHECK-NEXT: i32.shl
; CHECK-NEXT: i32.const 1
; CHECK-NEXT: i32.shr_s
; CHECK-NEXT: i32.ne
; CHECK:      select
  %1 = tail call { i31, i1 } @llvm.ssub.with.overflow.i31(i31 %a, i31 %b)
  %2 = extractvalue { i31, i1 } %1, 0
  %3 = extractvalue { i31, i1 } %1, 1
  %4 = select i1 %3, i31 %a, i31 %2
  ret i31 %4
}

define i32 @usub_i32(i32 %a, i32 %b) {
; CHECK-LABEL: usub_i32
; CHECK: local.get 0
; CHECK-NEXT: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: i32.sub
; CHECK-NEXT: local.tee [[VAR:[0-9]+]]
; CHECK-NEXT: local.get [[VAR]]
; CHECK-NEXT: local.get 0
; CHECK-NEXT: i32.gt_u
; CHECK:      select
  %1 = tail call { i32, i1 } @llvm.usub.with.overflow.i32(i32 %a, i32 %b)
  %2 = extractvalue { i32, i1 } %1, 0
  %3 = extractvalue { i32, i1 } %1, 1
  %4 = select i1 %3, i32 %a, i32 %2
  ret i32 %4
}

define i32 @ssub_i32(i32 %a, i32 %b) {
; CHECK-LABEL: ssub_i32
; CHECK: local.get 0
; CHECK-NEXT: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: i32.sub
; CHECK-NEXT: local.tee [[VAR:[0-9]+]]
; CHECK-NEXT: local.get 1
; CHECK-NEXT: i32.const 0
; CHECK-NEXT: i32.gt_s
; CHECK-NEXT: local.get [[VAR]]
; CHECK-NEXT: local.get 0
; CHECK-NEXT: i32.lt_s
; CHECK-NEXT: i32.xor
; CHECK:      select
  %1 = tail call { i32, i1 } @llvm.ssub.with.overflow.i32(i32 %a, i32 %b)
  %2 = extractvalue { i32, i1 } %1, 0
  %3 = extractvalue { i32, i1 } %1, 1
  %4 = select i1 %3, i32 %a, i32 %2
  ret i32 %4
}

define i64 @usub_i64(i64 %a, i64 %b) {
; CHECK-LABEL: usub_i64
; CHECK: local.get 0
; CHECK-NEXT: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: i64.sub
; CHECK-NEXT: local.tee [[VAR:[0-9]+]]
; CHECK-NEXT: local.get [[VAR]]
; CHECK-NEXT: local.get 0
; CHECK-NEXT: i64.gt_u
; CHECK:      select
  %1 = tail call { i64, i1 } @llvm.usub.with.overflow.i64(i64 %a, i64 %b)
  %2 = extractvalue { i64, i1 } %1, 0
  %3 = extractvalue { i64, i1 } %1, 1
  %4 = select i1 %3, i64 %a, i64 %2
  ret i64 %4
}

define i64 @ssub_i64(i64 %a, i64 %b) {
; CHECK-LABEL: ssub_i64
; CHECK: local.get 0
; CHECK-NEXT: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: i64.sub
; CHECK-NEXT: local.tee [[VAR:[0-9]+]]
; CHECK-NEXT: local.get 1
; CHECK-NEXT: i64.const 0
; CHECK-NEXT: i64.gt_s
; CHECK-NEXT: local.get [[VAR]]
; CHECK-NEXT: local.get 0
; CHECK-NEXT: i64.lt_s
; CHECK-NEXT: i32.xor
; CHECK:      select
  %1 = tail call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %a, i64 %b)
  %2 = extractvalue { i64, i1 } %1, 0
  %3 = extractvalue { i64, i1 } %1, 1
  %4 = select i1 %3, i64 %a, i64 %2
  ret i64 %4
}