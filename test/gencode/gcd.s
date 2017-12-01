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
gcd2_19:
	movq _R_1, _R_11
	movq _R_2, _R_10
	movq $0, _R_9
	cmpq _R_10, _R_9
	jne l_44
	movq _R_11, _R_0
	ret
l_44:
	sarq $63, _R_dx
	movq _R_11, _R_ax
	idivq _R_10
	movq _R_dx, _R_9
	movq _R_10, _R_1
	movq _R_9, _R_2
	jmp gcd2_19
gcd_29:
	movq _R_1, _R_11
	movq _R_2, _R_10
	cmpq _R_10, _R_11
	jne l_45
	movq _R_11, _R_1
	movq _R_10, _R_2
	jmp gcd2_19
l_45:
	movq _R_10, _R_1
	movq _R_11, _R_2
	jmp gcd2_19

	.globl _asm_main
_asm_main: # main entry point
	pushq %rbx
	pushq %r12
	pushq %r13
	pushq %r14
	pushq %r15
    # main program start
	movq $18, _R_11
	movq $24, _R_10
	movq _R_11, _R_1
	movq _R_10, _R_2
	call gcd_29
    # main program end
	movq _R_0, _R_ax
	popq %r15
	popq %r14
	popq %r13
	popq %r12
	popq %rbx
	ret
