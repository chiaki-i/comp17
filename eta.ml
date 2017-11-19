(* eta-ish conversion *)

open Knormal

(* g : Knormal.t -> (string * string) list -> Knormal.t *)
let rec g expr env =
  match expr with
    Number (num) -> expr
  | Real (f) -> expr
  | Variable (name) -> Variable (Env.get env name)
  | Op (name1, op, name2) -> Op (Env.get env name1, op, Env.get env name2)
  | IfEqual (name1, name2, expr3, expr4) ->
	IfEqual (Env.get env name1, Env.get env name2,
		 g expr3 env,
		 g expr4 env)
  | IfLess (name1, name2, expr3, expr4) ->
	IfLess (Env.get env name1, Env.get env name2,
		g expr3 env,
		g expr4 env)
  | Let ((name, typ), arg1, arg2) ->
    begin
      match arg2 with
      | Variable (var) ->
        if name = var then
          try g arg1 env with Env.UnboundVariable _ -> arg1
        else
          let new_env = Env.add env name var in
          let new_arg1 = try g arg1 env with Env.UnboundVariable _ -> arg1 in
          let new_arg2 = try g arg2 new_env with Env.UnboundVariable _ -> arg2 in 
          Let ((name, typ), new_arg1, new_arg2)
      | _ ->
        let new_arg1 = try g arg1 env with Env.UnboundVariable _ -> arg1 in
        let new_arg2 = try g arg2 env with Env.UnboundVariable _ -> arg2 in 
        Let ((name, typ), new_arg1, new_arg2)
    end
  | LetRec ((name, typ), args, expr1, expr2) ->
    LetRec ((name, typ), args, g expr1 env, g expr env)
  | Application (name, name_list) ->
    Application (Env.get env name,
                 List.map (fun var -> Env.get env var) name_list)

(* Eta.f : Knormal.t -> Knormal.t *)

let f expr = g expr Env.empty_env
