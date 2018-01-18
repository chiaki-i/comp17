(* 高階の言語にも対応したレジスタ割り当ての前処理P *)

(* g : Closure.t -> Closure.t *)
let rec g expr = match expr with
    Closure.Number (c) -> Closure.Number (c)
  | Closure.Real (c) -> Closure.Real (c)
  | Closure.Variable (x) -> Closure.Variable (x)
  | Closure.Op (arg1, op, arg2) -> Closure.Op (arg1, op, arg2)
  | Closure.IfEqual (arg1, arg2, arg3, arg4) ->
    Closure.IfEqual (arg1, arg2, g arg3, g arg4)
  | Closure.IfLess (arg1, arg2, arg3, arg4) ->
    Closure.IfLess (arg1, arg2, g arg3, g arg4)
  | Closure.Let ((name, typ), arg1, arg2) ->
    Closure.Let ((name, typ), g arg1, g arg2)
  | Closure.LetClosure ((name, typ), closure, arg) ->
    Closure.LetClosure ((name, typ), closure, g arg)
  | Closure.AppC (varname, strlist) ->
    (* appC_helper : string list -> string list -> int -> Closure.t *)
    let rec appC_helper args lst cnt = match args with
        [] -> let rev = List.rev lst in
        Closure.AppC (List.hd rev, List.tl rev)
      | first :: rest ->
        let next = cnt + 1 in
        let rn = Register.make_register next in
        Closure.Let ((rn, Type.gen_type ()), Closure.Variable (first),
                     appC_helper rest (rn :: lst) next) in
    appC_helper (varname :: strlist) [] Register.minimum
  | Closure.AppD (strlabel, strlist) ->
    (* appD_helper : string -> string list -> string list -> int -> Closure.t *)
    let rec appD_helper f args lst cnt = match args with
        [] -> Closure.AppD (f, List.rev lst)
      | first :: rest ->
        let next = cnt + 1 in
        let rn = Register.make_register next in
        Closure.Let ((rn, Type.gen_type ()), Closure.Variable (first),
                     appD_helper f rest (rn :: lst) next) in
    appD_helper strlabel strlist [] Register.minimum

(* g_def : Closure.def_t -> Closure.def_t *)
let rec g_def definition = match definition with
    Closure.FunDef ((f, typ), lst1, lst2, expr) ->
    (* make_Rs : (string * Type.t) list -> ((string * Type.t) * string) list -> int -> ((string * Type.t) * string) list
     * get r_lst from lst2 *)
    let rec make_Rs lst ans cnt = match lst with
        [] -> []
      | (vname, vtyp) as first :: rest ->
        let next = cnt + 1 in
        let rn = Register.make_register next in
        (first, rn) :: (make_Rs rest ans next) in
    let r_lst = make_Rs ((f, typ) :: lst2) [] Register.minimum in 
    (* def_helper2 : Closure.t -> ((string * Type.t), string) list -> Closure.t
     * - get expression using r_lst from lst2 *)
    let rec def_helper2 expr r_lst = match r_lst with
        [] -> g expr
      | ((vname, vtyp), rn) :: rest ->
        Closure.Let((vname, vtyp), Closure.Variable (rn), def_helper2 expr rest) in
    let rs = List.map (fun ((_, b), c) -> (c, b)) r_lst in
    Closure.FunDef ((f, typ), lst1, rs, def_helper2 expr r_lst)

(* g_program : Closure.prog_t -> Closure.prog_t *)
let rec g_program program = match program with
  Closure.Program (lst, expr) -> Closure.Program (List.map g_def lst, g expr)

(* 高階の言語にも対応したレジスタ割り当ての前処理への入り口 *)
let f expr = g_program expr
