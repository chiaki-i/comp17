(* Intel 用コード生成 *)

open First
open Register

(* registers *)

let r_sp = "_R_sp"
let r_bp = "_R_bp"
let r_ax = "_R_ax"
let r_dx = "_R_dx"

(* instructions *)

let label l =	  l ^ ":\n"
let movqi i r2  = "	movq $" ^ string_of_int i ^ ", " ^ r2 ^ "\n"
let movq r1 r2    = "	movq " ^ r1 ^ ", " ^ r2 ^ "\n"
let addq r1 r2  = "	addq " ^ r1 ^ ", " ^ r2 ^ "\n"
let subq r1 r2  = "	subq " ^ r1 ^ ", " ^ r2 ^ "\n"
let subqi i r2  = "	subq " ^ string_of_int i ^ ", " ^ r2 ^ "\n"
let imulq r1 r2 = "	imulq " ^ r1 ^ ", " ^ r2 ^ "\n"
let sarqi i r2 = "	sarq $" ^ string_of_int i ^ ", " ^ r2 ^ "\n"
let idivq r    = "	idivq " ^ r ^ "\n"
let cmpq r1 r2  = "	cmpq " ^ r1 ^ ", " ^ r2 ^ "\n"
let jne l =	"	jne " ^ l ^ "\n"
let jle l =	"	jle " ^ l ^ "\n"
let jmp l =	"	jmp " ^ l ^ "\n"
let pushq r =	"	pushq " ^ r ^ "\n"
let popq r  =	"	popq " ^ r ^ "\n"
let call f =	"	call " ^ f ^ "\n"
let ret =	"	ret\n"

(* headers *)

let top =	"	.text\n"
let middle =	"\n" ^
		"	.globl _asm_main\n" ^
		"_asm_main: # main entry point\n" ^
			pushq "%rbx" ^
			pushq "%r12" ^
			pushq "%r13" ^
			pushq "%r14" ^
			pushq "%r15" ^
			pushq r_bp ^
			movq r_sp r_bp ^
		"    # main program start\n"
let last =	"    # main program end\n" ^
			movq (make_register 0) r_ax ^
			movq r_bp r_sp ^
			popq r_bp ^
			popq "%r15" ^
			popq "%r14" ^
			popq "%r13" ^
			popq "%r12" ^
			popq "%rbx" ^
			ret

(* push/pop registers *)

let rec push_live live = match live with
    [] -> ""
  | var :: rest -> pushq var ^ push_live rest

let rec pop_live live = match live with
    [] -> ""
  | var :: rest -> pop_live rest ^ popq var

(* メイン *)

...

(* Code.f : First.prog_t -> string *)

let f program = ...
