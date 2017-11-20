(* 環境：
   変数 key の値が value であることを保持するテーブル *)

(* key が見つからなかったときに raise される例外 *)
exception UnboundVariable of string

(* 空の環境 *)
let empty_env = []

(* 環境 env に (key, value) を加える *)
let add env key value = (key, value) :: env

(* 環境 env から key に対応する値を取り出す *)
let rec get env key = match env with
    [] -> raise (UnboundVariable key)
  | (first_key, first_value) :: rest ->
	if key = first_key then first_value else get rest key
        
(* print current environment (= table of variables) : for debugging *)
(* print_env : (string * string) list -> unit *)
let print env =
  print_string "{ ";
  let rec print_env env = match env with
      [] -> print_string "";
    | (first, value) :: rest ->
      print_string (first ^ "=" ^ value ^ ", ");
      print_env rest in
  print_env env;
  print_string " }\n"
