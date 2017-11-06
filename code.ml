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

(* main *)
                     
exception NotSupported

(* del_v : string list -> string -> string list *)
let rec del_v lst v = match lst with
    [] -> []
  | first :: rest ->
    if first = v then del_v rest v else first :: del_v rest v

(* expand : string list -> string *)
let rec expand lst = match lst with
    [] -> ""
  | first :: rest -> first ^ (expand rest)

(* free_vars : First.t -> string list *)
let rec free_vars expr = match expr with
    First.Number (c) -> []
  | First.Variable (v) -> [v]
  | First.Op (v1, op, v2) -> [v1; v2]
  | First.IfEqual (v1, v2, e3, e4) -> (free_vars e3) @ (free_vars e4) @ [v1; v2]
  | First.IfLess (v1, v2, e3, e4) -> (free_vars e3) @ (free_vars e4) @ [v1; v2]
  | First.Let ((v, _), e1, e2) -> (free_vars e1) @ (del_v (free_vars e2) v)
  | First.Application (f, args) -> args
  | _ -> []
    
(* g : First.t -> string -> string list -> string *)
let rec g expr z live = match expr with
    First.Number (num) -> movqi num z
  | First.Real (f) -> raise NotSupported
  | First.Variable (v) ->
    if v = z then "" else movq v z
  | First.Op (v1, op, v2) ->
    begin
      match op with
        Operator.Plus -> (movq v1 r_ax) ^ (addq v2 r_ax) ^ (movq r_ax z)
      | Operator.Minus -> (movq v1 r_ax) ^ (subq v2 r_ax) ^ (movq r_ax z)
      | Operator.Times -> (movq v1 r_ax) ^ (imulq v2 r_ax) ^ (movq r_ax z)
      | Operator.Divide -> (sarqi 63 r_dx) ^ (movq v1 r_ax) ^ (idivq v2) ^ (movq v1 r_ax)
      | Operator.Mod -> (sarqi 63 r_dx) ^ (movq v1 r_ax) ^ (idivq v2) ^ (movq r_dx z)
      | _ -> raise NotSupported
    end
  | First.IfEqual (v1, v2, e3, e4) ->
    let l1 = Gensym.f "l" in
    let l2 = Gensym.f "l" in 
    (cmpq v1 v2) ^ (jne l1) ^ (g e3 z live) ^ (jmp l2) ^
      (label l1) ^ (g e4 z live) ^ (label l2)
  | First.IfLess (v1, v2, e3, e4) ->
    let l1 = Gensym.f "l" in
    let l2 = Gensym.f "l" in
    (cmpq v1 v2) ^ (jle l1) ^ (g e3 z live) ^ (jmp l2) ^
    (label l1) ^ (g e4 z live) ^ (label l2)
  | First.Let ((v, _), e1, e2) ->
    (g e1 v (live @ (del_v (free_vars e2) v))) ^ (g e2 z live) (* revise the case of e1 *)
  | First.Application (f, args) ->
    if z = (Register.make_register 0) then (push_live live) ^ (call f) ^ (pop_live live)
    else (push_live live) ^ (call f) ^ (movq (Register.make_register 0) z) ^ (pop_live live)

(* g_def : First.def_t -> string *)
let g_def definition = match definition with
  First.FunDef ((f, _), args, expr) ->
  (label f) ^ (pushq r_bp) ^ (movq r_sp r_bp) ^ (* local variables *)
  (g expr (Register.make_register 0) []) ^ (movq r_bp r_sp) ^ (popq r_bp) ^ ret
    
(* g_program : First.prog_t -> string *)
let g_program program = match program with
    First.Program (lst, expr) ->
    top ^ (expand (List.map g_def lst)) ^ middle ^ (g expr (Register.make_register 0) []) ^ last
    
(* Code.f : First.prog_t -> string *)

let f program = g_program program
