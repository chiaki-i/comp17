let rec f g = 3 in
let rec g x = f g in
g 2
