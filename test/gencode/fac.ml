(* compute factorial of 10 *)

let rec fac n =
  if n = 0 then 1
  else n * fac (n - 1)
in

fac 10 (* 3628800 *)
