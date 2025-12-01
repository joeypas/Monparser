module type T = sig
  (** ['a t] represents a monad with type ['a]. *)
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
end

module type S = sig
  include T

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
end


(** Functor building a monad structure *)
module Make (M : T) : S with type 'a t = 'a M.t = struct
  include M

  let seq p q = bind p (fun f -> map q (fun m -> f m))
  let map2 f m1 m2 = bind m1 (fun x -> map m2 (fun y -> f x y))
  let map3 f m1 m2 m3 = bind m1 (fun x -> bind m2 (fun y -> map m3 (fun z -> f x y z)))

  let map4 f m1 m2 m3 m4 =
    bind m1 (fun x -> bind m2 (fun y -> bind m3 (fun z -> map m4 (fun a -> f x y z a))))
  ;;

  let both (p : 'a t) (q : 'b t) : ('a * 'b) t = map2 (fun x y -> x, y) p q

  module Infix = struct
    let ( >>= ) = bind
    let ( >>| ) = map
    let ( <+> ) = plus
    let ( <$> ) g m = map m g
    let ( <*> ) = seq
    let ( *> ) p q = p >>= fun _ -> q
    let ( <* ) p q = p >>= fun r -> q >>| fun _ -> r
  end
end
