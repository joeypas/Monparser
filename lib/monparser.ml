include Monad

type pos = int * int
type pstring = pos * char list

module Parser =
  MakeReader
    (MaybeM)
    (struct
      type t = pstring
    end)
    (struct
      type t = pos
    end)

include Parser

(* infix operators *)
(* State helpers *)

let implode l = Core.String.of_char_list l
let explode = Core.String.to_list

let parse (p : 'a t) (s : string) =
  let res = p (0, 0) ((0, 0), explode s) in
  match res with
  | Some (x, (pos, xs)) ->
    x, "Consumed: " ^ string_of_int (snd pos) ^ ", Remaining: '" ^ implode xs ^ "'"
  | None -> failwith "Fatal"
;;

(* fixpoint combinator *)
let rec fix f x = f (fix f) x

let newstate ((l, c), xs) : pstring =
  let newpos = function
    | '\n' -> l + 1, 0
    | '\t' -> l, ((c / 8) + 1) * 8
    | _ -> l, c + 1
  in
  match xs with
  | x :: xs -> newpos x, xs
  | _ -> (l, c + 1), []
;;

let onside (l, c) (dl, dc) = c >= dc || l == dl

let off (p : 'a t) : 'a t =
  env
  >>= fun (_, dc) ->
  fetch >>= fun ((l, c), _) -> if c = dc then setenv (l, dc) p >>| fun v -> v else fail
;;

(* Single item parsers *)
let item =
  update newstate
  >>= fun (pos, xs) ->
  env
  >>= fun defpos ->
  if onside pos defpos
  then (
    match xs with
    | x :: _ -> return x
    | _ -> fail)
  else fail
;;

(* let item : char t = *)
(*   update List.tl *)
(*   >>= fun x -> *)
(*   try return (List.hd x) with *)
(*   | _ -> fail *)
(* ;; *)

let sat (f : char -> bool) = item >>= fun x -> if f x then return x else fail
let char c = sat (fun y -> c = y)
let both (p : 'a t) (q : 'b t) : ('a * 'b) t = (fun x y -> x, y) <$> p <*> q
let newl = char '\n' <* (fetch >>= fun ((l, c), _) -> setenv (l + 1, c) env)

let string s =
  let len = String.length s in
  let rec loop s i = if i >= len then return s else char s.[i] *> loop s (i + 1) in
  loop s 0
;;

(* Many item parsers *)
let many (p : 'a t) = fix (fun m -> List.cons <$> p <*> m <+> return [])
let many1 (p : 'a t) : 'a list t = List.cons <$> p <*> many p

let many1_offside (p : 'a t) : 'a list t =
  fetch >>= fun (pos, _) -> setenv pos (many1 (off p)) >>| fun vs -> vs
;;

let sepby1 p sep : 'a list t =
  (fun x xs -> x :: xs) <$> p <*> many ((fun _ y -> y) <$> sep <*> p)
;;

let take_while1 f : string t = (fun l -> implode l) <$> many1 (sat f)

let chainl1 p op =
  let rec rest acc = (fun f y -> f acc y) <$> op <*> p >>= rest <+> return acc in
  p >>= fun init -> rest init
;;

let rec chainr1 (p : 'a t) (op : ('a -> 'a -> 'a) t) =
  p >>= fun x -> (fun f y -> f x y) <$> op <*> chainr1 p op <+> return x
;;
