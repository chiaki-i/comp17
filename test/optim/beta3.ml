(* let rec の中で Subst *)
let rec f x y =
  let tmp1 = x in
  let tmp2 = y in
  if tmp1 < tmp2 then tmp1 else tmp2 in
f 1 2;;
