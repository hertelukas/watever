; RUN: watever -l 0 %s | wasm2wat - | FileCheck %s

target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-wasip2"

; Function Attrs: mustprogress norecurse
define hidden noundef i32 @__main_argc_argv(i32 noundef %0, ptr noundef readnone captures(none) %1) local_unnamed_addr #0 personality ptr @__gxx_wasm_personality_v0 {
; CHECK-LABEL: $__main_argc_argv
  invoke void @_Z3foov()
          to label %10 unwind label %3

3:                                                ; preds = %2
  %4 = catchswitch within none [label %5] unwind to caller

5:                                                ; preds = %3
  %6 = catchpad within %4 [ptr null]
  %7 = tail call ptr @llvm.wasm.get.exception(token %6)
  %8 = tail call i32 @llvm.wasm.get.ehselector(token %6)
  %9 = call ptr @__cxa_begin_catch(ptr %7) #3 [ "funclet"(token %6) ]
  call void @__cxa_end_catch() [ "funclet"(token %6) ]
  catchret from %6 to label %10

10:                                               ; preds = %2, %5
  ret i32 0
}

declare void @_Z3foov() local_unnamed_addr #1

declare i32 @__gxx_wasm_personality_v0(...)

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn
declare ptr @llvm.wasm.get.exception(token) #2

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn
declare i32 @llvm.wasm.get.ehselector(token) #2

declare ptr @__cxa_begin_catch(ptr) local_unnamed_addr

declare void @__cxa_end_catch() local_unnamed_addr

attributes #0 = { mustprogress norecurse "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="generic" "target-features"="+bulk-memory,+bulk-memory-opt,+call-indirect-overlong,+exception-handling,+multivalue,+mutable-globals,+nontrapping-fptoint,+reference-types,+sign-ext" }
attributes #1 = { "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="generic" "target-features"="+bulk-memory,+bulk-memory-opt,+call-indirect-overlong,+exception-handling,+multivalue,+mutable-globals,+nontrapping-fptoint,+reference-types,+sign-ext" }
attributes #2 = { mustprogress nocallback nofree nosync nounwind willreturn }
attributes #3 = { nounwind }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}
!llvm.errno.tbaa = !{!2}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 22.1.0-wasi-sdk (https://github.com/llvm/llvm-project 4434dabb69916856b824f68a64b029c67175e532)"}
!2 = !{!3, !3, i64 0}
!3 = !{!"int", !4, i64 0}
!4 = !{!"omnipotent char", !5, i64 0}
!5 = !{!"Simple C++ TBAA"}
