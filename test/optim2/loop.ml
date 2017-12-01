(* find greatest common divisor of 12 + 6 and 3 * 8 *)
let rec loop counter expr =
  if counter > 0 then expr + loop (counter - 1) expr
  else expr in

let rec gcd2 m n =
  if n = 0 then m
  else let amari = m mod n
       in gcd2 n amari
in

let rec gcd m n =
  if m <= n then gcd2 n m
	    else gcd2 m n
in

loop 100 (gcd (12 + 6) (3 * 8));;
