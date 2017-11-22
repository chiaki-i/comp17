(* LET associativity conversion *)

open Knormal

(* retrieve variable name : if not found, return key *)
(* get_var : (string * string) list -> string -> string *)
let get_var env key =
  try Env.get env key
  with Env.UnboundVariable _ -> key

(* g : Knormal.t -> (string * string) list -> Knormal.t *)
let rec g expr env =
  match expr with
  | Number (num) -> expr
  | Real (real) -> expr
  | Variable (var) -> Variable (get_var env var)
  | Op (name1, op, name2) -> Op (get_var env name1, op, get_var env name2)
  | IfEqual (name1, name2, expr3, expr4) ->
	IfEqual (get_var env name1, get_var env name2, g expr3 env, g expr4 env)
  | IfLess (name1, name2, expr3, expr4) ->
	IfLess (get_var env name1, get_var env name2, g expr3 env, g expr4 env)
  | Let ((x, typ_x), arg1, arg2) ->
    begin
      match arg1 with
        Let ((y, typ_y), new_arg1, new_arg2) ->
        let new_L = g new_arg1 env in
        let new_M = g new_arg2 env in
        let new_N = g arg2 env in
        Let ((y, typ_y), new_L, Let((x, typ_x), new_M, new_N))
      | _ -> Let ((x, typ_x), g arg1 env, g arg2 env)
    end
  | LetRec ((name, typ), args, expr1, expr2) ->
    LetRec ((name, typ), args, g expr1 env, g expr2 env)
  | Application (name, name_list) ->
    Application (get_var env name, List.map (fun var -> get_var env var) name_list)

(* Assoc.f : Knormal.t -> Knormal.t *)

let f expr = g expr Env.empty_env
