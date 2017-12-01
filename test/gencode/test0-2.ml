let rec fac x =
  if x = 1 then 1
  else fac (x - 1) * x
in fac 6
