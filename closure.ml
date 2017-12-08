(* Closure.t: クロージャ変換後の抽象構文木の型 *)

type closure_t = Cls of (string * Type.t) * (string * Type.t) list

type t = Number of int
       | Real of float
       | Variable of string
       | Op of string * Operator.t * string
       | IfEqual of string * string * t * t
       | IfLess of string * string * t * t
       | Let of (string * Type.t) * t * t
       | LetClosure of (string * Type.t) * closure_t * t
       | AppC of string (* 変数名 *) * string list
       | AppD of string (* ラベル名 *) * string list

type def_t = FunDef of (string * Type.t) * (string * Type.t) list *
                       (string * Type.t) list * t

type prog_t = Program of def_t list * t

(* Closure.print: 抽象構文木をプリントする関数（デバッグ用）  *)

let type_on = ref false (* 型推論を実装したら true にする *)

let indent i = String.make i ' '

let string_of_closure (Cls ((f, _), lst)) = match lst with
    [] -> "[" ^ f ^ "]"
  | (x, _) :: rest ->
    "[" ^ f ^ ", [" ^
    List.fold_left (fun str (y, _) -> str ^ "," ^ y)
               x
               rest
    ^ "]]"

let rec string_of_expr expr i = match expr with
    Number (num) -> string_of_int num
  | Real (f) -> string_of_float f
  | Variable (name) -> name
  | Op (arg1, op, arg2) ->
    "(" ^ arg1
        ^ (match op with
          Operator.Plus -> "+"
        | Operator.Minus -> "-"
        | Operator.Times -> "*"
        | Operator.Divide -> "/"
        | Operator.Mod -> " mod "
        | Operator.Residue -> " % "
        | Operator.PlusDot -> "+."
        | Operator.MinusDot -> "-."
        | Operator.TimesDot -> "*."
        | Operator.DivideDot -> "/.")
        ^ arg2 ^ ")"
  | IfEqual (arg1, arg2, arg3, arg4) ->
    "if " ^ arg1 ^ "=" ^ arg2 ^ "\n"
    ^ indent i ^ "then " ^ string_of_expr arg3 (i+5) ^ "\n"
    ^ indent i ^ "else " ^ string_of_expr arg4 (i+5)
  | IfLess (arg1, arg2, arg3, arg4) ->
    "if " ^ arg1 ^ "<" ^ arg2 ^ "\n"
    ^ indent i ^ "then " ^ string_of_expr arg3 (i+5) ^ "\n"
    ^ indent i ^ "else " ^ string_of_expr arg4 (i+5)
  | Let ((name, t), arg1, arg2) ->
        (if !type_on
     then "let (" ^ name ^ ":" ^ Type.to_string t ^ ")="
     else "let " ^ name ^ "=")
    ^ string_of_expr arg1 (i+5+String.length name)
    ^ " in\n"
    ^ indent i ^ string_of_expr arg2 i
  | LetClosure ((name, t), closure, arg) ->
        (if !type_on
     then "let_closure (" ^ name ^ ":" ^ Type.to_string t ^ ")="
     else "let_closure " ^ name ^ "=")
    ^ string_of_closure closure
    ^ " in\n"
    ^ indent i ^ string_of_expr arg i
  | AppC (name, args) ->
    "[" ^ name ^ " " ^
    (match args with
        [] -> ""
      | arg :: args ->
        arg ^
        List.fold_right (fun a rest -> " " ^ a ^ rest) args "")
    ^ "]"
  | AppD (name, args) ->
    "(" ^ name ^ " " ^
    (match args with
        [] -> ""
      | arg :: args ->
        arg ^
        List.fold_right (fun a rest -> " " ^ a ^ rest) args "")
    ^ ")"

let rec string_of_def (FunDef ((f, _), fvs, params, body)) =
  match fvs with
      [] -> "let rec " ^ f ^ " " ^
          List.fold_left (fun str (x, t) ->
        if !type_on
        then str ^ "(" ^ x ^ " : " ^ Type.to_string t ^ ") "
        else str ^ x ^ " ")
          ""
          params ^ "=\n  " ^
        string_of_expr body 2 ^ "\n"
    | (y, t) :: rest ->
        "let rec " ^
        f ^ List.fold_left (fun str (y, t) ->
          if !type_on
          then str ^ ", " ^ y ^ " : " ^ Type.to_string t
          else str ^ "," ^ y)
        (" [" ^ if !type_on then y ^ " : " ^ Type.to_string t else y)
        rest ^ "] " ^
        List.fold_left (fun str (x, t) ->
          if !type_on
          then str ^ "(" ^ x ^ " : " ^ Type.to_string t ^ ") "
          else str ^ x ^ " ")
        ""
        params ^ "=\n  " ^
          string_of_expr body 2 ^ "\n"

let rec string_of_prog (Program (def_list, expr)) =
  "{\n"
   ^ List.fold_left (fun str def -> str ^ string_of_def def)
            "" def_list
   ^ "}\n"
   ^ string_of_expr expr 0

let print prog =
  let str = string_of_prog prog
  in (print_string str;
      print_newline ())

(* expressionに含まれている自由変数を計算するための関数群 *)

(* del_v : string list -> string -> string list *)
let rec del_v lst v = match lst with
    [] -> []
  | first :: rest ->
    if first = v then del_v rest v else first :: del_v rest v

(* del_vlst : string list -> string list -> string list *)
let rec del_vlst lst vlst = match vlst with
    [] -> []
  | first :: rest ->
    let next = del_v lst first in del_vlst next rest
(*
(* free_vars : First.t -> string list *)
let rec free_vars expr = match expr with
    First.Number (c) -> []
  | First.Variable (v) -> [v]
  | First.Op (v1, op, v2) -> [v1; v2]
  | First.IfEqual (v1, v2, e3, e4) -> (free_vars e3) @ (free_vars e4) @ [v1; v2]
  | First.IfLess (v1, v2, e3, e4) -> (free_vars e3) @ (free_vars e4) @ [v1; v2]
  | First.Let ((v, _), e1, e2) -> (free_vars e1) @ (del_v (free_vars e2) v)
  | First.Application (f, args) -> args
  | _ -> []
*)

(* del_v_cls : (string * Type.t) list -> (string * Type.t) -> (string * Type.t) list *)
let rec del_v_cls lst (v, typ_v) = match lst with
    [] -> []
  | (name, typ) as first :: rest ->
    if name = v then del_v_cls rest (v, typ_v)
    else first :: del_v_cls rest (v, typ_v)

(* del_vlst_cls : (string * Type.t) list -> (string * Type.t) list -> (string * Type.t) list *)
let rec del_vlst_cls lst vlst = List.map (del_v_cls lst) vlst

(* freevars_cls : Closure.t -> string list *)
let rec freevars_cls expr = match expr with
    Number (n) -> []
  | Real (f) -> []
  | Variable (v) -> [v]
  | Op (v1, op, v2) -> [v1; v2]
  | IfEqual (v1, v2, arg1, arg2) ->
    (freevars_cls arg1) @ (freevars_cls arg2) @ [v1; v2]
  | IfLess (v1, v2, arg1, arg2) ->
    (freevars_cls arg1) @ (freevars_cls arg2) @ [v1; v2]
  | Let ((name, typ), arg1, arg2) -> (freevars_cls arg1) @ (del_v (freevars_cls arg2) name)
  | LetClosure ((name, _), Cls (_, cls), arg2) ->
    let str_cls = List.map (fun (a, b) -> a) cls in 
    (del_v (str_cls @ (freevars_cls arg2)) name)
  | AppC (var, args) -> var :: args
  | AppD (_, args) -> args

(* クロージャ変換のメイン *)

(* def_list : (Closure.def_t list) ref のつもり *)
let def_list = ref []

(* g : Knormal.t -> Closure.prog_t *)
let rec g expr vars = match expr with (* verbose version *)
    Knormal.Number (num) -> Number (num)
  | Knormal.Real (f) -> Real (f)
  | Knormal.Variable (name) -> Variable (name)
  | Knormal.Op (arg1, op, arg2) -> Op (arg1, op, arg2)
  | Knormal.IfEqual (arg1, arg2, arg3, arg4) ->
    IfEqual (arg1, arg2, g arg3 vars, g arg4 vars)
  | Knormal.IfLess (arg1, arg2, arg3, arg4) ->
    IfLess (arg1, arg2, g arg3 vars, g arg4 vars)
  | Knormal.Let ((name, typ), arg1, arg2) ->
    Let ((name, typ), g arg1 vars, g arg2 vars)
  | Knormal.LetRec ((name, typ), args, arg1, arg2) ->
    begin
      let new_arg1 = g arg1 vars in
      let new_arg2 = g arg2 vars in
      let str_args = List.map (fun (a, b) -> a) args in
      let vlst = name :: str_args in 
      let fv = (del_vlst (freevars_cls new_arg1) vlst) in
      let with_typ = List.map (fun x -> (x, (Type.gen_type ()))) fv in
      def_list := FunDef ((name, typ), with_typ, args, new_arg1) :: !def_list;
      LetClosure ((name, typ), Cls ((name, typ), with_typ), new_arg2)
    end
  | Knormal.Application (name, args) ->
    if List.mem name vars then AppC (name, args) else AppD (name, args)

(* Closure.g_program : Knormal.t -> Closure.prog_t *)
let rec g_program expr =
  begin
    def_list := [];
    let new_expr = g expr [] in
    Program (!def_list, new_expr)
  end

(* Closure.f: クロージャ変換の入口 *)
(* Closure.f : Knormal.t -> Closure.prog_t *)
let f program = g_program program
