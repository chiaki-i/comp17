(* simple register allocation *)
open Print

(* g : Closure.t -> (string * string) -> int -> Closure.t *)
let rec g expr env cnt =
  match expr with
    Closure.Number (c) -> Closure.Number (c)
  | Closure.Real (c) -> Closure.Real (c)
  | Closure.Variable (x) ->
    begin
      try Closure.Variable (Env.get env x)
      with _ -> Closure.Variable (x)
    end
  | Closure.Op (arg1, op, arg2) -> Closure.Op (Env.get env arg1, op, Env.get env arg2)
  | Closure.IfEqual (arg1, arg2, arg3, arg4) ->
    Closure.IfEqual (Env.get env arg1, Env.get env arg2, g arg3 env cnt, g arg4 env cnt)
  | Closure.IfLess (arg1, arg2, arg3, arg4) ->
    Closure.IfLess (Env.get env arg1, Env.get env arg2, g arg3 env cnt, g arg4 env cnt)
  | Closure.Let ((name, typ), arg1, arg2) ->
      if Register.is_register name then
        Closure.Let ((name, typ), g arg1 env cnt, g arg2 env cnt)
      else
        let next = cnt - 1 in
        let new_register = Register.make_register next in
        let new_env = Env.add env name new_register in
        Closure.Let((new_register, typ), g arg1 env next, g arg2 new_env next)
  | Closure.LetClosure ((name, typ), Closure.Cls ((f, ftyp), vns), expr) ->
    begin
      let next = cnt - 1 in
      let new_register = Register.make_register next in
      let new_env = Env.add env name new_register in
      let new_vns = List.map (fun (x, y) -> (Env.get new_env x, y)) vns in
      let new_expr = g expr new_env next in
      Closure.LetClosure ((new_register, typ),
                          Closure.Cls ((f, ftyp), new_vns), new_expr)
    end
  | Closure.AppC (varname, strlist) -> expr
  | Closure.AppD (strlabel, strlist) -> expr

(* g_def : Closure.def_t -> int -> (string * string) list -> Closure.def_t *)
let rec g_def env definition = match definition with
    Closure.FunDef ((f, ftyp), ylst, rlst, expr) ->
    begin
      let rec make_new_ylst_env ylst result_ylst env cnt = match ylst with
          [] -> (result_ylst, env)
        | (name, typ) :: rest ->
          let next_cnt = cnt + 1 in
          let new_register = Register.make_register next_cnt in
          let new_env = Env.add env name new_register in
          let new_ylst = (new_register, typ) :: result_ylst in
          make_new_ylst_env rest new_ylst new_env next_cnt in
      let (new_ylst, new_env) = make_new_ylst_env ylst [] env Register.minimum in
      let new_expr = g expr new_env Register.limit in
      Closure.FunDef ((f, ftyp), new_ylst, rlst, new_expr)
    end
    
(* g_program : Closure.prog_t -> (string * string) list -> Closure.prog_t *)
let rec g_program program = match program with
    Closure.Program (lst, expr) ->
    Closure.Program (List.map (g_def Env.empty_env) lst,
                   g expr Env.empty_env Register.limit) 

(* レジスタ割り当て処理への入口 *)
let f expr = g_program expr
