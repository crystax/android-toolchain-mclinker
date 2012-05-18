; RUN: cp %p/portable_f.s ./portable_f.ll
; RUN: cp %p/true_f.s ./true_f.ll

; RUN: %MCLinker -mtriple="arm-none-linux-gnueabi" -march=arm \
; RUN: -filetype=obj -fPIC -dB %s -o %t.1.o
; RUN: %MCLinker -mtriple="arm-none-linux-gnueabi" -march=arm \
; RUN: -filetype=obj -fPIC -dB ./portable_f.ll -o %t.2.o
; RUN: %MCLinker -mtriple="arm-none-linux-gnueabi" -march=arm \
; RUN: -filetype=obj -fPIC -dB ./true_f.ll -o %t.3.o
; RUN: %MCLinker -mtriple="arm-none-linux-gnueabi" -march=arm \
; RUN: %t.1.o %t.2.o %t.3.o -o %t.4.o --portable f
target triple = "arm-none-linux-gnueabi"

define i8* @g(i32 %c) nounwind uwtable ssp {
entry:
  %c.addr = alloca i32, align 4
  store i32 %c, i32* %c.addr, align 4
  %0 = load i32* %c.addr, align 4
  %call = call i8* @f(i32 %0)
  ret i8* %call
}

declare i8* @f(i32)
