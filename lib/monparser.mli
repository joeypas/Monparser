
(** type of the state/input *)
type s

(** type of the base monad (list, option, ...) *)
type 'a m

(**
  type of the parser

  'a parser = input -> ('a * input')
*)
type 'a t

(** [return v] lifts [v] to the monadic context. *)
val return : 'a -> 'a t

(** [fail] is the empty or zero value. *)
val fail : 'a t

(**
  [bind m f] applies [f] to [m].

  [f] is a function that accepts a value and returns a monad.
*)
val bind : 'a t -> ('a -> 'b t) -> 'b t

(** [map m f] applies [f] to [m]. *)
val map : 'a t -> ('a -> 'b) -> 'b t

(** [plus p q] runs [p] and if it failed, runs [q]. *)
val plus : 'a t -> 'a t -> 'a t

(** [update f] applies the function [f] to the state *)
val update : (s -> s) -> s t

(** [set s] sets the state to [s] *)
val set : s -> s t

(** [fetch] returns the current state *)
val fetch : s t

(* [item] consumes a single character from the input and returns it *)
val item : char t

(** [sat f] parses any character when [f] returns [true] and returns it *)
val sat : (char -> bool) -> char t

(** [char c] parses the character [c] and returns it *)
val char : char -> char t

(** [string s] parses the string [s] and returns it *)
val string : string -> string t

(** [fix f] finds the fixpoint of f and runs the resulting parser *)
val fix : ('a t -> 'a t) -> 'a t

(** [many p] runs the parser [p] zero or more times and returns a list of successful results *)
val many : 'a t -> 'a list t

(** [many1 p] runs the parser [p] one or more times and returns a list of successful results *)
val many1 : 'a t -> 'a list t

(**
     [sepby1 p sep] runs the parser [p] one or more times, running the parser [sep] in between
     each run of [p] and returns a list of successful results
  *)
val sepby1 : 'a t -> _ t -> 'a list t

(** [take_while1 f] consumes characters while the condition [f] is true, and returns a string of parsed characters *)
val take_while1 : (char -> bool) -> string t

(** 
    [ops ps] tries to run the parser from each pair in the list [ps] discarding the result of the 
    first successful parse and instead returning a value of type ['b].

    [ps] is a list of pairs where the first element is a parser and the second is an element of type ['b]
   *)
val ops : ('a t * 'b) list -> 'b t

(** [any ps] tries to run each parser in the list [ps] returning the first successful parse *)
val any : 'a t list -> 'a t

(**
    [chainl1 p op] runs parser [p] one or more times, seperated by operators that associate to the left. 

    [op] is a function that accepts two items of type ['a] and returns an item of type ['a].
  *)
val chainl1 : 'a t -> ('a -> 'a -> 'a) t -> 'a t

(**
    [chainr1 p op] runs parser [p] one or more times, seperates by operators that associate to the right.

    [op] is a function that accepts two items of type ['a] and returns an item of type ['a].
  *)
val chainr1 : 'a t -> ('a -> 'a -> 'a) t -> 'a t

type 'a parse_error =
  | Fail
  | Partial of 'a * string

(** [parse p s] runs the parser [p] on the string [s] *)
val parse : 'a t -> string -> ('a, 'a parse_error) result


val seq : ('a -> 'b) t -> 'a t -> 'b t
val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val map3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
val map4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t
val both : 'a t -> 'b t -> ('a * 'b) t

module Infix : sig
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( <+> ) : 'a t -> 'a t -> 'a t
  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

  (** [p *> q] is the same as [p >>= fun _ -> q] *)
  val ( *> ) : 'a t -> 'b t -> 'b t
  (** [p <* q] is the same as [p >>= fun r -> q >>| r] *)
  val ( <* ) : 'a t -> 'b t -> 'a t
end
