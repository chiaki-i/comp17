(* (非常に単純な)レジスタ割り当てA. 
   環境の変数テーブルは、変数とその変数に割り当てられているレジスタのペアのリストになっている. *)
(* ここでは、Env.get は環境から変数名をkeyとして、valueであるレジスタ名を返す
   Env.get : (string * string) list -> string -> string *)

(* g : First.t -> (string * string) list -> int -> First.t *)
let rec g expr env cnt = match expr with
    First.Number (num) -> First.Number (num)
  | First.Real (f) -> First.Real (f)
  | First.Variable (name) ->
    if Register.is_register name then First.Variable (name)
    else First.Variable (Env.get env name)
  | First.Op (arg1, op, arg2) ->
    First.Op (Env.get env arg1, op, Env.get env arg2)
  | First.IfEqual (arg1, arg2, arg3, arg4) ->
    First.IfEqual (Env.get env arg1, Env.get env arg2, g arg3 env cnt, g arg4 env cnt)
  | First.IfLess (arg1, arg2, arg3, arg4) ->
    First.IfLess (Env.get env arg1, Env.get env arg2, g arg3 env cnt, g arg4 env cnt)
  | First.Let ((name, typ), arg1, arg2) ->
    if Register.is_register name then
      First.Let ((name, typ), g arg1 env cnt, g arg2 env cnt)
    else
      let next = (cnt - 1) mod Register.limit in
      let new_register = Register.make_register next in
      let new_env = Env.add env name new_register in
      First.Let((new_register, typ), g arg1 env next, g arg2 new_env next) 
  | First.Application (name, arg2) -> First.Application (name, arg2)

(* g_def : First.def_t -> (string * string) list -> First.def_t *)
let rec g_def env cnt definition = match definition with
    First.FunDef ((name, typ), args, expr) ->
    First.FunDef ((name, typ), args, g expr env Register.limit)
    
(* g_program : First.prog_t -> (string * string) list -> First.prog_t *)
let rec g_program program = match program with
    First.Program (lst, expr) ->
    First.Program (List.map (g_def Env.empty_env Register.limit) lst,
                   g expr Env.empty_env Register.limit) 

(* レジスタ割り当て処理への入口 *)
let f expr = g_program expr
