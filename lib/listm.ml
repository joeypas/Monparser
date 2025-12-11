type 'a t = 'a list

let return v = [ v ]
let fail = []

let rec bind m f =
  match m with
  | [] -> []
  | x :: xs -> f x @ bind xs f
;;

let rec map m f =
  match m with
  | [] -> []
  | x :: xs -> f x :: map xs f
;;

let plus p q =
  match p with
  | [] -> q
  | x :: xs -> x :: (xs @ q)
;;
