#define _R_ax %rax
#define _R_0  %rbx
#define _R_1  %rcx
#define _R_dx %rdx
#define _R_2  %rsi
#define _R_3  %rdi
#define _R_bp %rbp
#define _R_sp %rsp
#define _R_4  %r8
#define _R_5  %r9
#define _R_6  %r10
#define _R_7  %r11
#define _R_8  %r12
#define _R_9  %r13
#define _R_10 %r14
#define _R_11 %r15
	.text
f:
	pushq _R_bp
	movq _R_sp, _R_bp
	movq _R_1, _R_11
	movq _R_2, _R_10
	movq $3, _R_9
	movq _R_11, _R_8
	cmpq _R_9, _R_8
	jle l_15
	movq _R_11, _R_7
	movq _R_10, _R_6
	movq _R_7, _R_ax
	imulq _R_6, _R_ax
	movq _R_ax, _R_0
	jmp l_16
l_15:
	movq $10, _R_7
	movq _R_10, _R_6
	cmpq _R_7, _R_6
	jle l_17
	movq _R_11, _R_5
	movq _R_10, _R_4
	movq _R_5, _R_ax
	addq _R_4, _R_ax
	movq _R_ax, _R_0
	jmp l_18
l_17:
	movq _R_11, _R_4
	movq _R_10, _R_3
	movq _R_4, _R_ax
	imulq _R_3, _R_ax
	movq _R_ax, _R_5
	movq _R_10, _R_4
	movq _R_5, _R_ax
	imulq _R_4, _R_ax
	movq _R_ax, _R_0
l_18:
l_16:
	movq _R_bp, _R_sp
	popq _R_bp
	ret

	.globl _asm_main
_asm_main: # main entry point
	pushq %rbx
	pushq %r12
	pushq %r13
	pushq %r14
	pushq %r15
	pushq _R_bp
	movq _R_sp, _R_bp
    # main program start
	movq $2, _R_11
	movq $9, _R_10
	movq _R_11, _R_1
	movq _R_10, _R_2
	call f
    # main program end
	movq _R_0, _R_ax
	movq _R_bp, _R_sp
	popq _R_bp
	popq %r15
	popq %r14
	popq %r13
	popq %r12
	popq %rbx
	ret
