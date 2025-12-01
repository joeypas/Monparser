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

let newstate (((l, c), xs) : s) : s =
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
(** [item] returns the next character *)
let item : char t =
  update newstate
  >>= fun (_, xs) ->
  match xs with
  | x :: _ -> return x
  | _ -> fail
;;

(** [sat f] accepts any character when [f] returns [true] and returns it *)
let sat (f : char -> bool) : char t = item >>= fun x -> if f x then return x else fail

(** [char c] accepts the character [c] and returns it *)
let char c = sat (fun y -> c = y)

(** [string s] accepts the string [s] and returns it *)
let string s : string t =
  let len = String.length s in
  let rec loop s i = if i >= len then return s else char s.[i] *> loop s (i + 1) in
  loop s 0
;;

(* Many item parsers *)

(** [many p] runs the parser [p] zero or more times and returns a list of successful results *)
let many (p : 'a t) : 'a list t = fix (fun m -> map2 List.cons p m <+> return [])

(** [many1 p] runs the parser [p] one or more times and returns a list of successful results *)
let many1 (p : 'a t) : 'a list t = map2 List.cons p (many p)

(**
   [sepby1 p sep] runs the parser [p] one or more times, running the parser [sep] in between
   each run of [p] and returns a list of successful results
*)
let sepby1 (p : 'a t) (sep : _ t) : 'a list t =
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

type 'a parse_error =
  | Fail
  | Partial of 'a * string

(** [parse p s] runs the parser [p] on the string [s] *)
let parse (p : 'a t) (s : string) : ('a, 'a parse_error) result =
  let value = p ((0, 0), explode s) in
  match value with
  | Some (x, (pos, xs)) ->
    if List.is_empty xs
    then
      Error
        (Partial (x, "Consumed: " ^ show_pos pos ^ ", Remaining: '" ^ implode xs ^ "'"))
    else Ok x
  | None -> Error Fail
;;
