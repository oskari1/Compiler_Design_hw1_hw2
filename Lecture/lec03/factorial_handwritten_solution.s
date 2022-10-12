# long factorial(long i) {
#     if (i > 1l)
#         return i * factorial(i - 1l);
#     return 1l;
# }


.text
.globl factorial

factorial:
    #i is in %rdi

    #boilerplate
    pushq   %rbp 
    movq    %rsp, %rbp

#   if (i > 1l)
    cmpq    $1, %rdi
    jg      .REC

    movq    $1, %rax
    jmp     .RET

.REC:
#   i * factorial(i - 1l);
    pushq  %rdi
    subq   $1, %rdi
    call   factorial
    popq   %rdi
    imulq  %rdi, %rax

.RET:
    movq    %rbp, %rsp
    popq    %rbp
    ret #return value in %rax

.data
