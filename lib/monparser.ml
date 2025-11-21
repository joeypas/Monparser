type pos = int * int [@@deriving show]

module State = struct
  type t = pos * char list [@@deriving show]
end

module Parser = Statem.Make (Optionm.S) (State)
include Parser
include Infix

(* State helpers *)

let implode l = Core.String.of_char_list l
let explode = Core.String.to_list

(* fixpoint combinator *)
let rec fix f x = f (fix f) x

let newstate ((l, c), xs) : State.t =
  let newpos = function
    | '\n' -> l + 1, 0
    | '\t' -> l, ((c / 8) + 1) * 8
    | _ -> l, c + 1
  in
  match xs with
  | x :: xs -> newpos x, xs
  | _ -> (l, c + 1), []
;;

(* Single item parsers *)
let item : char t =
  update newstate
  >>= fun (_, xs) ->
  match xs with
  | x :: _ -> return x
  | _ -> fail
;;

let sat (f : char -> bool) : char t = item >>= fun x -> if f x then return x else fail
let char c = sat (fun y -> c = y)

let string s : string t =
  let len = String.length s in
  let rec loop s i = if i >= len then return s else char s.[i] *> loop s (i + 1) in
  loop s 0
;;

(* Many item parsers *)
let many (p : 'a t) : 'a list t = fix (fun m -> List.cons <$> p <*> m <+> return [])
let many1 (p : 'a t) : 'a list t = List.cons <$> p <*> many p

let sepby1 p sep : 'a list t =
  map2 (fun x xs -> x :: xs) p (many (map2 (fun _ y -> y) sep p))
;;

let take_while1 (f : char -> bool) : string t = (fun l -> implode l) <$> many1 (sat f)

let chainl1 (p : 'a t) (op : ('a -> 'a -> 'a) t) : 'a t =
  let rec rest acc = map2 (fun f y -> f acc y) op p >>= rest <+> return acc in
  p >>= fun init -> rest init
;;

let rec chainr1 (p : 'a t) (op : ('a -> 'a -> 'a) t) : 'a t =
  p >>= fun x -> map2 (fun f y -> f x y) op (chainr1 p op) <+> return x
;;

let parse (p : 'a t) (s : string) =
  let value = p ((0, 0), explode s) in
  match value with
  | Some (x, (pos, xs)) ->
    x, "Consumed: " ^ show_pos pos ^ ", Remaining: '" ^ implode xs ^ "'"
  | None -> failwith "Error"
;;
