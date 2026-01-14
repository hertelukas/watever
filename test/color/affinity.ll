; RUN: watever -l 5 %s -o /dev/zero | FileCheck %s

; Affinities are sorted from high to low; failing to assign the first two
; values the same local is more costly than assigning the second the
; same color.

declare i32 @sink(i32 %val)

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

define i32 @loop_broken_interference(i32 %a, i32 %n) {
; CHECK-LABEL: Coloring function loop_broken_interference
; CHECK: Affinities
; Highest priority should be assigning c and b the same local, as this move
; would be required with every loop iteration.
; CHECK-NEXT: From c to b
; CHECK-NEXT: From a to b
entry:
  br label %loop
loop:
  %b = phi i32 [ %a, %entry ], [ %c, %loop ]
; Break tree here -> b and c no longer interfere
  %c = call i32 @sink(i32 %b)
  %cond = icmp eq i32 %n, %c
  br i1 %cond, label %loop, label %exit
exit:
  ret i32 %c
}

define i32 @test_affinity_forced(i32 %start, i32 %n) {
; CHECK-LABEL: Coloring function test_affinity_forced
; CHECK: Affinities
; CHECK-NEXT: From next to i
; CHECK-NEXT: From start to i
; CHECK: Merging chunks next and i

entry:
  br label %loop

loop:
  %i = phi i32 [ %start, %entry ], [ %next, %loop ]
  ; Break the lifetime, so %next and %i do not interfere
  %tmp = call i32 @sink(i32 %i)
  %next = add i32 %tmp, 1
  ; Create interference between %next and %start
  ; (in order for the coalescer to prefer %next over %start)
  call i32 @sink(i32 %start)
  %cond = icmp eq i32 %next, %n
  br i1 %cond, label %loop, label %exit

exit:
  ret i32 %next
}