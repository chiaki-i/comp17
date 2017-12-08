let rec twice f =
  let rec g x = f (f x) in
  g
in

let rec double x = x + x
in

(twice double) 2
