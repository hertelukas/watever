; RUN: watever -l 5 %s -o /dev/zero | FileCheck %s

declare i32 @start_live()

define i32 @arg_live(i32 %arg) {
; CHECK-LABEL: liveness for arg_live
; CHECK: block entry
; CHECK-NEXT: Live In {
; CHECK-NEXT: %arg
; CHECK: Live Out {
; CHECK-NEXT: }
entry:
  ret i32 %arg
}

define i32 @live_across_blocks() {
; CHECK-LABEL: liveness for live_across_blocks
;
; CHECK: block entry
; CHECK-NEXT: Live In {
; CHECK-NEXT: }
; CHECK: Live Out {
; CHECK-NEXT: %called
; CHECK-NEXT: }
;
; CHECK: block loop
; CHECK-NEXT: Live In {
; CHECK-NEXT: %called
; CHECK-NEXT: }
; CHECK: Live Out {
; CHECK-NEXT: %called
; CHECK-NEXT: }
;
; CHECK: block end
; CHECK-NEXT: Live In {
; CHECK-NEXT: %called
; CHECK-NEXT: }
; CHECK: Live Out {
; CHECK-NEXT: }
entry:
  %called = call i32 @start_live()
  br label %loop
loop:
  %2 = add i32 %called, 1
  %3 = icmp eq i32 %2, 5
  br i1 %3, label %loop, label %end
end:
  ret i32 %called
}

define i32 @phi_simple(i1 %cond, i32 %a, i32 %b) {
; CHECK-LABEL: liveness for phi_simple
;
; Check that the PHI operands are LiveOut but not LiveIn on merge block.
;
; CHECK: block lhs
; CHECK: Live Out {
; CHECK-NEXT: %val_a
; CHECK-NEXT: }
;
; CHECK: block rhs
; CHECK: Live Out {
; CHECK-NEXT: %val_b
; CHECK-NEXT: }
;
; CHECK: block merge
; CHECK: Live In {
; CHECK-NEXT: %res
; CHECK-NEXT: }
entry:
  br i1 %cond, label %lhs, label %rhs

lhs:
  %val_a = add i32 %a, 10
  br label %merge

rhs:
  %val_b = add i32 %b, 20
  br label %merge

merge:
  %res = phi i32 [ %val_a, %lhs ], [ %val_b, %rhs ]
  ret i32 %res
}

define i32 @phi_loop(i32 %n) {
; CHECK-LABEL: liveness for phi_loop
;
; Check live across back edges
;
; CHECK: block loop
; CHECK-NEXT: Live In {
; CHECK-NEXT: %i
; CHECK-NEXT: %n
; CHECK-NEXT: }
; CHECK: Live Out {
; CHECK-NEXT: %i
; CHECK-NEXT: %n
; CHECK-NEXT: %next
; CHECK-NEXT: }
entry:
  br label %loop

loop:
  %i = phi i32 [ 0, %entry ], [ %next, %loop ]
  %next = add i32 %i, 1
  %cond = icmp slt i32 %next, %n
  br i1 %cond, label %loop, label %exit

exit:
  ret i32 %i
}

define i32 @if_else_bypass(i1 %cond, i32 %x) {
; CHECK-LABEL: liveness for if_else_bypass
;
; Check that %x is live through the else block, even though unused
;
; CHECK: block else
; CHECK-NEXT: Live In {
; CHECK-NEXT: %x
; CHECK-NEXT: }
; CHECK: Live Out {
; CHECK-NEXT: %x
; CHECK-NEXT: }
entry:
  br i1 %cond, label %if, label %else

if:
  %y = add i32 %x, 1
  br label %merge

else:
; x has to be live here, as it flows from entry to merge
  br label %merge

merge:
  %z = add i32 %x, 5
  ret i32 %z
}

define i32 @promoted_alloca(i1 %cond, i32 %val) {
; CHECK-LABEL: liveness for promoted_alloca
;
; Promoted allocas live from the NCD of their users to their last
;
; CHECK: block entry
; CHECK-NEXT: Live In {
; CHECK-NEXT: %cond
; CHECK-NEXT: %val
; CHECK-NEXT: }
; CHECK: Live Out {
; CHECK-NEXT: %cond
; CHECK-NEXT: %ptr2
; CHECK-NEXT: }
;
; CHECK: block head
; CHECK-NEXT: Live In {
; CHECK-NEXT: %cond
; CHECK-NEXT: %ptr2
; CHECK-NEXT: }
; CHECK: Live Out {
; CHECK-NEXT: %ptr1
; CHECK-NEXT: %ptr2
; CHECK-NEXT: }
;
; CHECK: block if
; CHECK-NEXT: Live In {
; CHECK-NEXT: %ptr1

; CHECK: Live Out {
; CHECK-NEXT: %ptr1
; CHECK-NEXT: }
;
; CHECK: block else
; CHECK-NEXT: Live In {
; CHECK-NEXT: %ptr1
; CHECK-NEXT: %ptr2
; CHECK-NEXT: }
; CHECK: Live Out {
; CHECK-NEXT: %ptr1
; CHECK-NEXT: }
;
; CHECK: block merge
; CHECK-NEXT: Live In {
; CHECK-NEXT: %ptr1
; CHECK-NEXT: }
; CHECK: Live Out {
; CHECK-NEXT: }
entry:
  %ptr1 = alloca i32
  %ptr2 = alloca i32
  store i32 %val, ptr %ptr2
  br label %head

head:
  br i1 %cond, label %if, label %else

if:
  store i32 2, ptr %ptr1
  br label %merge

else:
  %0 = load i32, ptr %ptr2
  store i32 %0, ptr %ptr1
  br label %merge

merge:
  %res = load i32, ptr %ptr1
  ret i32 %res
}