(* for debugging *)

let rec lst_string lst = match lst with
    [] -> ""
  | x :: xs -> (x ^ ", " ^ lst_string xs)

let rec lst_string_string lst = match lst with
    [] -> ""
  | (a, b) :: rest -> (a ^ "=" ^ b ^ ", " ^ (lst_string_string rest))

let rec lst_string_typt lst = match lst with
    [] -> ""
  | (a, _) :: rest -> (a ^ ", " ^ (lst_string_typt rest))

let print_lst1 name lst =
  begin
    print_string (name ^ ": [ ");
    print_string (lst_string lst);
    print_string " ]\n";
  end

let print_lst2 name lst =
  begin
    print_string (name ^ ": [ ");
    print_string (lst_string_string lst);
    print_string " ]\n";
  end

let print_lst3 name lst =
  begin
    print_string (name ^ ": [ ");
    print_string (lst_string_typt lst);
    print_string " ]\n";
  end
