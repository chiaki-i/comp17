(* i を受け取り _R_i を作る *)
(* make_register : int -> string *)
let make_register i = "_R_" ^ (string_of_int i)

(* j を受け取り _F_j を作る *)
(* make_fregister : int -> string *)
let make_fregister j = "_F_" ^ (string_of_int j)

(* 変数名がレジスタかどうかを判定する *)
(* 浮動小数点レジスタでも true を返す *)
(* is_register : string -> bool *)
let is_register r = String.get r 0 = '_'

(* 変数名が浮動小数点レジスタかどうかを判定する *)
(* is_fregister : string -> bool *)
let is_fregister f = String.get f 0 = '_' && String.get f 1 = 'F'

(* レジスタの番号は_R_1から始まる *)
let minimum = 0
 
(* レジスタの最大個数11個 *)
let limit = 11

(* レジスタ名から番号を抜き出す *)
let extract_num r =
  let length = String.length r in 
  if length = 4 then int_of_string (Char.escaped (String.get r 3))
  else int_of_string ((Char.escaped (String.get r 3)) ^
                      (Char.escaped (String.get r 4)))

exception NoAvailableRegister
(* 使用可能なレジスタ番号のうち最も大きいものを返す *)
let rec max_num lst cnt =
  if cnt = 0 then raise NoAvailableRegister else
  if List.mem cnt lst then max_num lst (cnt - 1)
  else cnt
