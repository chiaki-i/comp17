let rec f p q r s t = p + q * r / s - t in
let a = 3 in
let b = 2 in
let c = 4 in
let d = 2 in
let e = 0 in
if 3.0 +. (2.0 *. 4.0 /.2.0) -. 0.0 = 2.99 then 1
else if 0 <> 0 then 2
else if 2 <= 0 then 3
else if 2 >= 5 then 4
else if 2 < 0 - 2 then 5
else if 4 > 9 then (f a b c d e) mod 7
else f a b c d e
