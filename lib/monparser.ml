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

(** [newstate prev] gives us the next state of the parser based on the previous state [prev] *)
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
(** [item] returns the next character and updates the state *)
let item : char t =
  update newstate
  >>= fun (_, xs) ->
  match xs with
  | x :: _ -> return x
  | _ -> fail
;;

(** [sat f] parses any character when [f] returns [true] and returns it *)
let sat (f : char -> bool) : char t = item >>= fun x -> if f x then return x else fail

(** [char c] parses the character [c] and returns it *)
let char c = sat (fun y -> c = y)

(** [string s] parses the string [s] and returns it *)
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

(** [take_while1 f] consumes characters while the condition [f] is true, and returns a string of parsed characters *)
let take_while1 (f : char -> bool) : string t = (fun l -> implode l) <$> many1 (sat f)

(**  *)
let ops (ps : ('a t * 'b) list) : 'b t =
  let map_ignore p v = p >>| fun _ -> v in
  match ps with
  | [] -> fail
  | (p, op) :: rest ->
    let start = map_ignore p op in
    List.fold_left (fun acc (p, op) -> acc <+> map_ignore p op) start rest
;;

(** [any ps] tries to run each parser in the list [ps] returning the first successful parse *)
let any (ps : 'a t list) : 'a t =
  match ps with
  | [] -> fail
  | p :: rest -> List.fold_left ( <+> ) p rest
;;

(**
  [chainl1 p op] runs parser [p] one or more times, seperated by operators that associate to the left. 

  [op] is a function that accepts two items of type ['a] and returns an item of type ['a].
*)
let chainl1 (p : 'a t) (op : ('a -> 'a -> 'a) t) : 'a t =
  let rec rest acc = map2 (fun f y -> f acc y) op p >>= rest <+> return acc in
  p >>= rest
;;

(**
  [chainr1 p op] runs parser [p] one or more times, seperates by operators that associate to the right.

  [op] is a function that accepts two items of type ['a] and returns an item of type ['a].
*)
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
    then Ok x
    else
      Error
        (Partial (x, "Consumed: " ^ show_pos pos ^ ", Remaining: '" ^ implode xs ^ "'"))
  | None -> Error Fail
;;
