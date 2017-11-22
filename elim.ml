(* dead code elimination *)

open Knormal

(* retrieve variable name : if not found, return key *)
(* get_var : (string * string) list -> string -> string *)
let get_var env key =
  try Env.get env key
  with Env.UnboundVariable _ -> key

(* 変数 x が expr の中で使われているかどうか判定する *)
(* is_used : string -> Knormal.t -> bool *)
let rec is_used x expr = match expr with
    Number (num) -> false
  | Real (real) -> false
  | Variable (var) -> x = var
  | Op (name1, op, name2) -> (x = name1) || (x = name2)
  | IfEqual (name1, name2, arg1, arg2) ->
    (x = name1) || (x = name2) || (is_used x arg1) || (is_used x arg2)
  | IfLess (name1, name2, arg1, arg2) ->
    (x = name1) || (x = name2) || (is_used x arg1) || (is_used x arg2)
  | Let ((name, typ), arg1, arg2) ->
    (is_used x arg1) || (x != name && is_used x arg2)
  | LetRec ((name, typ), args, arg1, arg2) ->
    let bool_list = List.map (fun (item, _) -> x != item) args in
    let not_included = (List.fold_left (||) false bool_list) || x != name  in
    (not_included && is_used x arg1) || (x != name && is_used x arg2)
  | Application (name, name_list) ->
    let bool_list = List.map (fun item -> x = item) name_list in
    List.fold_left (||) false bool_list

(* expr が自明かどうか判定する *)
(* is_trivial : Knormal.t -> bool *)
let rec is_trivial expr = match expr with
    Number (num) -> true
  | Real (real) -> true
  | Variable (var) -> true
  | Op (name1, op, name2) -> true
  | IfEqual (name1, name2, arg1, arg2) -> is_trivial arg1 && is_trivial arg2
  | IfLess (name1, name2, arg1, arg2) -> is_trivial arg1 && is_trivial arg2
  | Let ((name, typ), arg1, arg2) -> false
  | LetRec ((name, typ), args, arg1, arg2) -> false
  | Application (name, name_list) -> false

(* g : Knormal.t -> Knormal.t *)
let rec g expr = match expr with
    Number (num) -> expr
  | Real (real) -> expr
  | Variable (var) -> expr
  | Op (name1, op, name2) -> expr
  | IfEqual (name1, name2, arg1, arg2) ->
    IfEqual (name1, name2, g arg1, g arg2)
  | IfLess (name1, name2, arg1, arg2) ->
    IfLess (name1, name2, g arg1, g arg2)
  | Let ((name, typ), arg1, arg2) ->
    let new_arg1 = g arg1 in
    let new_arg2 = g arg2 in
    if (is_trivial new_arg1 && not (is_used name new_arg2))
    then new_arg2 (* discard *)
    else Let ((name, typ), new_arg1, new_arg2)
  | LetRec ((name, typ), args, arg1, arg2) ->
    let new_arg1 = g arg1 in
    let new_arg2 = g arg2 in
    if not (is_used name new_arg2) then new_arg2
    else LetRec ((name, typ), args, new_arg1, new_arg2)
  | Application (name, name_list) -> expr

(* Beta.f : Knormal.t -> Knormal.t *)

let f expr = g expr
