(* constant folding *)

open Knormal

(* try to find var_name in env; if not found, return false *)
(* exist_in_env : (string * 'a) list -> string -> bool *)
(* investigate : (string * int) list -> string -> Knormal.t *)
let investigate env key = 
  let exist_in_env env name =
    try let _ = Env.get env name in true
    with Env.UnboundVariable _ -> false in
  if exist_in_env env key then Number (Env.get env key)
  else Variable (key)

(* g : Knormal.t -> (string * int) -> Knormal.t *)
let rec g expr env = match expr with
    Number (num) -> expr
  | Real (real) -> expr
  | Variable (var) -> investigate env var
  | Op (name1, op, name2) ->
    (* new_name1, new_name2 : Knormal.t *)
    let new_name1 = investigate env name1 in
    let new_name2 = investigate env name2 in
    begin
      match (new_name1, op, new_name2) with
        (Number (num1), Operator.Plus, Number (num2)) -> Number (num1 + num2)
      | (Number (num1), Operator.Minus, Number (num2)) -> Number (num1 - num2)
      | (Number (num1), Operator.Times, Number (num2)) -> Number (num1 * num2)
      | (Number (num1), Operator.Divide, Number (num2)) -> Number (num1 / num2)
      | (Number (num1), Operator.Mod, Number (num2)) -> Number (num1 mod num2)
      | (_, _, _) -> expr
    end
  | IfEqual (name1, name2, arg1, arg2) ->
    (* new_name1, new_name2 : Knormal.t *)
    let new_name1 = investigate env name1 in
    let new_name2 = investigate env name2 in
    begin
      match (new_name1, new_name2) with
        (Number (num1), Number (num2)) ->
        if num1 = num2 then (g arg1 env) else (g arg2 env)
      | _ -> expr
    end
  | IfLess (name1, name2, arg1, arg2) ->
    (* new_name1, new_name2 : Knormal.t *)
    let new_name1 = investigate env name1 in
    let new_name2 = investigate env name2 in
    begin
      match (new_name1, new_name2) with
        (Number (num1), Number (num2)) ->
        if num1 < num2 then (g arg1 env) else (g arg2 env)
      | _ -> expr
    end
  | Let ((name, typ), arg1, arg2) ->
    begin
      match arg1 with
        Number (num) ->
        let new_env = Env.add env name num in
        g arg2 new_env
      | _ -> Let ((name, typ), g arg1 env, g arg2 env)
    end
  | LetRec ((name, typ), args, arg1, arg2) ->
    LetRec ((name, typ), args, g arg1 env, g arg2 env)
  | Application (name, name_list) -> expr

(* Beta.f : Knormal.t -> Knormal.t *)

let f expr = g expr Env.empty_env
