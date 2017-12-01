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
fac:
	pushq _R_bp
	movq _R_sp, _R_bp
	movq _R_1, _R_11
	movq _R_11, _R_10
	movq $0, _R_9
	cmpq _R_10, _R_9
	jne l_9
	movq $1, _R_0
	jmp l_10
l_9:
	movq _R_11, _R_8
	movq _R_11, _R_5
	movq $1, _R_4
	movq _R_5, _R_ax
	subq _R_4, _R_ax
	movq _R_ax, _R_6
	movq _R_6, _R_1
	pushq _R_8
	call fac
	movq _R_0, _R_7
	popq _R_8
	movq _R_8, _R_ax
	imulq _R_7, _R_ax
	movq _R_ax, _R_0
l_10:
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
	movq $10, _R_11
	movq _R_11, _R_1
	call fac
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
