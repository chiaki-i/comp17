(* β conversion *)

open Knormal

(* g : Knormal.t -> (string * string) list -> Knormal.t *)
let rec g expr env =
  match expr with
  | Number (num) -> expr
  | Real (real) -> expr
  | Variable (var) -> Variable (Env.get env var)
  | Op (name1, op, name2) ->
    Op (Env.get env name1, op, Env.get env name2)
  | IfEqual (name1, name2, expr3, expr4) ->
	IfEqual (Env.get env name1, Env.get env name2, g expr3 env, g expr4 env)
  | IfLess (name1, name2, expr3, expr4) ->
	IfLess (Env.get env name1, Env.get env name2, g expr3 env, g expr4 env)
  | Let ((new_name, typ), arg1, arg2) ->
    begin
      match arg1 with
        Variable (var) ->
        begin
          try (* look up for the oldest variable when nested *)
            let source = Env.get env var in
            let new_env = Env.add env new_name source in
            g arg2 new_env 
          with Env.UnboundVariable _ -> (* just add to ENV if no older variables *)
            let new_env = Env.add env new_name var in
            g arg2 new_env
        end
      | _ ->
        begin
          try
            Let ((new_name, typ), g arg1 env, g arg2 env)
          with Env.UnboundVariable _ -> (* no further beta conversions *)
            expr
        end
    end
  | LetRec ((name, typ), args, expr1, expr2) ->
    LetRec ((name, typ), args, g expr1 env, g expr env)
  | Application (name, name_list) ->
    Application (Env.get env name,
                         List.map (fun var -> Env.get env var) name_list)

(* Beta.f : Knormal.t -> Knormal.t *)

let f expr = g expr Env.empty_env
