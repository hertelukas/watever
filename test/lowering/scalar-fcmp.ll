; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

define i1 @fcmp_false(float %a, float %b) {
; CHECK-LABEL: fcmp_false
; CHECK: i32.const 0
    %1 = fcmp false float %a, %b
    ret i1 %1
}

define i1 @fcmp_oeq(float %a, float %b) {
; CHECK-LABEL: fcmp_oeq
; CHECK: local.get 0
; CHECK-NEXT: local.get 1
; CHECK-NEXT: f32.eq
    %1 = fcmp oeq float %a, %b
    ret i1 %1
}

define i1 @fcmp_ogt(float %a, float %b) {
; CHECK-LABEL: fcmp_ogt
; CHECK:         local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    f32.gt
    %1 = fcmp ogt float %a, %b
    ret i1 %1
}

define i1 @fcmp_oge(float %a, float %b) {
; CHECK-LABEL: fcmp_oge
; CHECK:         local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    f32.ge
    %1 = fcmp oge float %a, %b
    ret i1 %1
}

define i1 @fcmp_olt(float %a, float %b) {
; CHECK-LABEL: fcmp_olt
; CHECK:         local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    f32.lt
    %1 = fcmp olt float %a, %b
    ret i1 %1
}

define i1 @fcmp_ole(float %a, float %b) {
; CHECK-LABEL: fcmp_ole
; CHECK:         local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    f32.le
    %1 = fcmp ole float %a, %b
    ret i1 %1
}

define i1 @fcmp_one(float %a, float %b) {
; CHECK-LABEL: fcmp_one
; CHECK:         local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    f32.gt
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    f32.lt
; CHECK-NEXT:    i32.or
    %1 = fcmp one float %a, %b
    ret i1 %1
}

define i1 @fcmp_ord(float %a, float %b) {
; CHECK-LABEL: fcmp_ord
; CHECK:         local.get 0
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    f32.eq
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    f32.eq
; CHECK-NEXT:    i32.and
    %1 = fcmp ord float %a, %b
    ret i1 %1
}

define i1 @fcmp_ueq(float %a, float %b) {
; CHECK-LABEL: fcmp_ueq
; CHECK:         local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    f32.gt
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    f32.lt
; CHECK-NEXT:    i32.or
; CHECK-NEXT:    i32.const 1
; CHECK-NEXT:    i32.xor
    %1 = fcmp ueq float %a, %b
    ret i1 %1
}

define i1 @fcmp_ugt(float %a, float %b) {
; CHECK-LABEL: fcmp_ugt
; CHECK:         local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    f32.le
; CHECK-NEXT:    i32.const 1
; CHECK-NEXT:    i32.xor
    %1 = fcmp ugt float %a, %b
    ret i1 %1
}

define i1 @fcmp_ult(float %a, float %b) {
; CHECK-LABEL: fcmp_ult
; CHECK:         local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    f32.ge
; CHECK-NEXT:    i32.const 1
; CHECK-NEXT:    i32.xor
    %1 = fcmp ult float %a, %b
    ret i1 %1
}

define i1 @fcmp_ule(float %a, float %b) {
; CHECK-LABEL: fcmp_ule
; CHECK:         local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    f32.gt
; CHECK-NEXT:    i32.const 1
; CHECK-NEXT:    i32.xor
    %1 = fcmp ule float %a, %b
    ret i1 %1
}

define i1 @fcmp_une(float %a, float %b) {
; CHECK-LABEL: fcmp_une
; CHECK:         local.get 0
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    f32.ne
    %1 = fcmp une float %a, %b
    ret i1 %1
}

define i1 @fcmp_uno(float %a, float %b) {
; CHECK-LABEL: fcmp_uno
; CHECK:         local.get 0
; CHECK-NEXT:    local.get 0
; CHECK-NEXT:    f32.ne
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    local.get 1
; CHECK-NEXT:    f32.ne
; CHECK-NEXT:    i32.or
    %1 = fcmp uno float %a, %b
    ret i1 %1
}

define i1 @fcmp_true(float %a, float %b) {
; CHECK-LABEL: fcmp_true
; CHECK: i32.const 1
    %1 = fcmp true float %a, %b
    ret i1 %1
}

