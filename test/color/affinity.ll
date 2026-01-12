; RUN: watever -l 5 %s -o /dev/zero | FileCheck %s

; Affinities are sorted from high to low; failing to assign the first two
; values the same local is more costly than assigning the second the
; same color.

define i32 @loop(i32 %a, i32 %n) {
; CHECK-LABEL: Coloring function loop
; CHECK: Affinities
; Highest priority should be assigning c and b the same local, as this move
; would be required with every loop iteration.
; CHECK-NEXT: From c to b
; CHECK-NEXT: From a to b
entry:
  br label %loop
loop:
  %b = phi i32 [ %a, %entry ], [ %c, %loop ]
  %c = add i32 %b, 1
  %cond = icmp eq i32 %n, %c
  br i1 %cond, label %loop, label %exit
exit:
  ret i32 %c
}