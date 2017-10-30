(* 1階の言語に対するレジスタ割り当ての前処理P *)

(* g : First.t -> First.t *)
let rec g expr = match expr with
    First.Number (num) -> First.Number (num)
  | First.Real (f) -> First.Real (f)
  | First.Variable (name) -> First.Variable (name)
  | First.Op (arg1, op, arg2) -> First.Op (arg1, op, arg2)
  | First.IfEqual (arg1, arg2, arg3, arg4) ->
    First.IfEqual (arg1, arg2, g arg3, g arg4)
  | First.IfLess (arg1, arg2, arg3, arg4) ->
    First.IfLess (arg1, arg2, g arg3, g arg4)
  | First.Let ((name, typ), arg1, arg2) ->
    First.Let ((name, typ), g arg1, g arg2)
  | First.Application (name, arg2) ->
    (* app_helper : string -> string list -> int -> First.t *)
    let rec app_helper f args lst cnt = match args with
        [] -> First.Application (name, List.rev lst)
      | first :: rest ->
        let next = cnt + 1 in
        let rn = Register.make_register next in
        First.Let ((rn, Type.gen_type ()), First.Variable(first),
                   app_helper f rest (rn :: lst) next) in 
    app_helper name arg2 [] Register.minimum
  
(* g_def : First.def_t -> First.def_t *)
let rec g_def definition = match definition with First.FunDef ((name, typ), args, expr) ->
    (* def_first_helper : (string * Type.t) list -> (string * Type.t) list -> First.t -> int -> First.t *)
    let rec def_first_helper args lst expr cnt = match args with
        [] -> g expr
      | (str_arg, typ_arg) :: rest ->
        let next = cnt + 1 in
        let rn = Register.make_register next in
        First.Let((str_arg, typ_arg), First.Variable(rn), def_first_helper rest lst expr next) in
    (* def_args_helper : (string * Type.t) list -> int -> (string * Type.t) list *)
    let rec def_args_helper args cnt = match args with
        [] -> []
      | (_, typ_arg) :: rest ->
        let next = cnt + 1 in
        let rn = Register.make_register next in
        (rn, typ_arg) :: def_args_helper rest next in
    First.FunDef ((name, Type.gen_type()), def_args_helper args Register.minimum,
                  def_first_helper args [] expr Register.minimum)

(* g_program : First.prog_t -> First.prog_t *)
let rec g_program program = match program with
    First.Program (lst, expr) -> First.Program (List.map g_def lst, g expr)

(* 1階の言語に対するレジスタ割り当ての前処理への入口 *)
let f expr = g_program expr
