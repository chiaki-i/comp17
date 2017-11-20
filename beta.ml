(* β conversion *)

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
  | Let ((new_name, typ), arg1, arg2) ->
    begin
      match arg1 with
        Variable (var) -> (* look up for the oldest variable when nested *)
        let new_env = Env.add env new_name (get_var env var) in
        g arg2 new_env
      | _ -> (* no further β conversions *)
        Let ((new_name, typ), g arg1 env, g arg2 env)
    end
  | LetRec ((name, typ), args, arg1, arg2) ->
    let new_arg1 = g arg1 env in
    let new_arg2 = g arg2 env in 
    LetRec ((name, typ), args, new_arg1, new_arg2)
  | Application (name, name_list) ->
    Application (get_var env name, List.map (fun var -> get_var env var) name_list)

(* Beta.f : Knormal.t -> Knormal.t *)

let f expr = g expr Env.empty_env
