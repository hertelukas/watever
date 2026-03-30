; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-unknown"

define i32 @write_after_read(ptr %ptr) {
; CHECK-LABEL: write_after_read
; CHECK:      local.get       0
; CHECK-NEXT: i32.load
; CHECK-NEXT: local.set       1
; CHECK-NEXT: local.get       0
; CHECK-NEXT: i32.const       1
; CHECK-NEXT: i32.store
; CHECK-NEXT: local.get       1
entry:
  %a = load i32, ptr %ptr
  store i32 1, ptr %ptr
  ret i32 %a
}

define i32 @mod_call_site(ptr %ptr) {
  store i32 0, ptr %ptr
  ret i32 0
}

define void @no_mod_ref_call_site(ptr %ptr) memory(none) {
  ret void
}

define void @ref_call_site(ptr %ptr) memory(read) {
  %res = load i32, ptr %ptr
  ret void
}

; The load can safely be reordered after the call,
; as %ptr and the call site are NoModRef
define i32 @legal_reorder_nomodref(ptr %ptr) {
; CHECK-LABEL: legal_reorder_nomodref
; CHECK-NEXT: local.get 0
; CHECK-NEXT: call
; CHECK-NEXT: local.get 0
; CHECK-NEXT: load
entry:
  %a = load i32, ptr %ptr
  call void @no_mod_ref_call_site(ptr %ptr)
  ret i32 %a
}

; The load can safely be reordered after the call,
; as %ptr and the call site are Ref
define i32 @legal_reorder_ref(ptr %ptr) {
; CHECK-LABEL: legal_reorder_ref
; CHECK-NEXT: local.get 0
; CHECK-NEXT: call
; CHECK-NEXT: local.get 0
; CHECK-NEXT: load
entry:
  %a = load i32, ptr %ptr
  call void @ref_call_site(ptr %ptr)
  ret i32 %a
}

; The load cannot be reordered after the call,
; as %ptr and the call site are ModRef
define i32 @illegal_reorder_call_site(ptr %ptr) {
; CHECK-LABEL: illegal_reorder_call_site
; CHECK: local.get 0
; CHECK-NEXT: i32.load
; CHECK-NEXT: local.set 1
; CHECK-NEXT: local.get 0
; CHECK-NEXT: call
; CHECK-NEXT: drop
; CHECK-NEXT: local.get 1
entry:
  %a = load i32, ptr %ptr
  call i32 @mod_call_site(ptr %ptr)
  ret i32 %a
}

; The load has to be scheduled before the call. This might be obvious,
; but the scheduler will try to evaluate %last_use after the call, so
; it is on top of the stack for the division.
; Hence, just checking that all users of a load come before a clobbering
; instruction is not enough - that would be true here, the last_use is coming
; before alias_call. The loads' root (if it were not itself a root) must come
; before any instruction potentially modifying the location.
define i32 @illegal_due_to_root_scheduling(ptr %ptr) {
; CHECK: i32.load
; Either do the addition here, or after the call
; CHECK: local.set 1
; CHECK: call
; CHECK: local.get 1
; CHECK: i32.div_u
entry:
  %l = load i32, ptr %ptr
  %last_use = add i32 %l, 1
  %alias_call = call i32 @mod_call_site(ptr %ptr) 
  %root = udiv i32 %alias_call, %last_use
  ret i32 %root
}