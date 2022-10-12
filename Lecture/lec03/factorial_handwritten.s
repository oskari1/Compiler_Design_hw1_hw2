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

    movq    %rbp, %rsp
    popq    %rbp
    ret #return value in %rax

.data
