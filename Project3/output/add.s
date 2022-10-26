	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 17
	.syntax unified
	.globl	_main                   @ -- Begin function main
	.p2align	2
	.code	32                      @ @main
_main:
@ %bb.0:
	mov	r0, #14
	mov	r1, #0
	bx	lr
                                        @ -- End function

.subsections_via_symbols
