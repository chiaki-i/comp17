(* (非常に単純な)レジスタ割り当てA. 
   環境の変数テーブルは、変数とその変数に割り当てられているレジスタのペアのリストになっている. *)
(* ここでは、Env.get は環境から変数名をkeyとして、valueであるレジスタ名を返す
   Env.get : (string * string) list -> string -> string *)
open Closure
open Print

(* 関数を呼び出した結果は r0 に入る *)
let r0 = "_R_0";;

(* interfere の初期値は空集合 *)
let empty = [];;

(* 同じレジスタに割り当てたい変数のペアのリストを作る *)
(* target : string -> First.t -> (string * string) list *)
let rec target z expr = match expr with
    Number (num) -> []
  | Real (real) -> []
  | Variable (var) -> [(z, var)]
  | Op (name1, op, name2) -> [] (* optional: +, -, * *)
  | IfEqual (name1, name2, arg1, arg2) -> (target z arg1) @ (target z arg2)
  | IfLess (name1, name2, arg1, arg2) -> (target z arg1) @ (target z arg2)
  | Let ((name, typ), arg1, arg2) -> (target z arg1) @ (target z arg2)
  | Application (name, name_list) -> [(z, r0)]

(* 同じレジスタに割り当ててはいけない変数のペアのリストを作る *)
(* interfere : string list -> First.t -> (string * string) list *)
let rec interfere live expr = match expr with
    Number (num) -> [] 
  | Real (real) -> []
  | Variable (var) -> []
  | Op (name1, op, name2) -> []
  | IfEqual (name1, name2, arg1, arg2) ->
    (interfere live arg1) @ (interfere live arg2)
  | IfLess (name1, name2, arg1, arg2) ->
    (interfere live arg1) @ (interfere live arg2)
  | Let ((name, typ), arg1, arg2) ->
    let new_live = live @ (free_vars expr) in
    (interfere new_live arg1) @ (List.map (fun z -> (name, z)) new_live)
    @ (interfere live arg2)
  | Application (name, name_list) -> []

               
(* alloc_one : string -> [env] (string * string) list -> 
               [target] (string * string) list -> 
               [interfere] (string * string) list -> string *)
let rec alloc_one v env t_lst i_lst =
  if Register.is_register v then v
  else
    (* alloc_helper : (string * string) list -> (string * string) -> string list *)
    (* judge : (string * string) list -> (string * string) -> bool *)
    let alloc_helper lst =
      let judge lst (y, r) = 
        if Register.is_register r
        then List.mem (y, v) lst || List.mem (v, y) lst
        else false in
      let source = List.filter (judge lst) env in
      List.map (fun (_, r) -> r) source in
    let prohibited = alloc_helper i_lst in
    let prohibited_num = List.map Register.extract_num prohibited in 
    let recommended = alloc_helper t_lst in
    let nominee = List.filter (fun x -> not (List.mem x prohibited)) recommended in
    begin
      (* print_lst1 "proh" prohibited;
         print_lst1 "recm" recommended;
         print_lst1 "nomi" nominee; *)
      match nominee with
        [] -> (let next = Register.max_num prohibited_num Register.limit in
               (* print_string  ((string_of_int next) ^ " "); *)
               Register.make_register next)
      | x :: _ -> x
    end

(* g : First.t -> (string * string) list -> int -> First.t *)
let rec g expr env t_lst i_lst = match expr with
    Number (num) -> Number (num)
  | Real (f) -> Real (f)
  | Variable (name) ->
    if Register.is_register name then Variable (name)
    else Variable (Env.get env name)
  | Op (arg1, op, arg2) ->
    Op (Env.get env arg1, op, Env.get env arg2)
  | IfEqual (arg1, arg2, arg3, arg4) ->
    IfEqual (Env.get env arg1, Env.get env arg2,
             g arg3 env t_lst i_lst, g arg4 env t_lst i_lst)
  | IfLess (arg1, arg2, arg3, arg4) ->
    IfLess (Env.get env arg1, Env.get env arg2,
            g arg3 env t_lst i_lst, g arg4 env t_lst i_lst)
  | Let ((name, typ), arg1, arg2) ->
    if Register.is_register name then
      Let ((name, typ), g arg1 env t_lst i_lst, g arg2 env t_lst i_lst)
    else
      (* let new_register = Register.make_register next in *)
      let new_register = alloc_one name env t_lst i_lst in
      let new_env = Env.add env name new_register in
      Let((new_register, typ), g arg1 env t_lst i_lst,
          g arg2 new_env t_lst i_lst)
  | Application (name, arg2) -> Application (name, arg2)
                                  
(* g_def : (string * string) list -> First.def_t -> First.def_t *)
let rec g_def env definition = match definition with
    FunDef ((name, typ), args, expr) ->
    let target_lst = target r0 expr in
    let interfere_lst = interfere empty expr in
    begin
      (* print_lst2 "t_lst" target_lst;
         print_lst2 "i_lst" interfere_lst; *)
      FunDef ((name, typ), args, g expr env target_lst interfere_lst)
    end
    
(* g_program : prog_t -> prog_t *)
let rec g_program program =
  match program with
    Program (lst, expr) -> (* lst: First.def_t list, expr: First.t *)
    let target_lst = target r0 expr in
    let interfere_lst = interfere empty expr in
    begin
      (* print_lst2 "t_lst" target_lst;
         print_lst2 "i_lst" interfere_lst; *)
      Program (List.map (g_def Env.empty_env) lst,
               g expr Env.empty_env target_lst interfere_lst)
    end

(* レジスタ割り当て処理への入口 *)
let f expr = g_program expr
