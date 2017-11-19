(* LET associativity conversion *)

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
  | Let ((x, typ_x), arg1, en) ->
    begin
      match arg1 with
        Let ((y, typ_y), el, em) ->
        let new_L = try g el env with Env.UnboundVariable _ -> el in
        let new_M = try g em env with Env.UnboundVariable _ -> em in
        let new_N = try g en env with Env.UnboundVariable _ -> en in
        Let ((y, typ_y), new_L, Let((x, typ_x), new_M, new_N))
      | _ ->
        let new_arg1 = try g arg1 env with Env.UnboundVariable _ -> arg1 in
        let new_arg2 = try g en env with Env.UnboundVariable _ -> en in 
        Let ((x, typ_x), new_arg1, new_arg2)
    end
  | LetRec ((name, typ), args, expr1, expr2) ->
    LetRec ((name, typ), args, g expr1 env, g expr env)
  | Application (name, name_list) ->
    Application (Env.get env name,
                         List.map (fun var -> Env.get env var) name_list)

(* Assoc.f : Knormal.t -> Knormal.t *)

let f expr = g expr Env.empty_env
