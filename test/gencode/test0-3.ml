let rec f x y =
  if x > 3 then x * y
  else if y > 10 then x + y
  else x * y * y
in f 2 9
