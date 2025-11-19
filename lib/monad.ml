module type T = sig
  type 'a t

  val return : 'a -> 'a t
  val fail : 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val map : 'a t -> ('a -> 'b) -> 'b t
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
    val ( *> ) : 'a t -> 'b t -> 'b t
    val ( <* ) : 'a t -> 'b t -> 'a t
  end

  module Let_syntax : sig
    module Let_syntax : sig
      val return : 'a -> 'a t
      val map : 'a t -> f:('a -> 'b) -> 'b t
      val bind : 'a t -> f:('a -> 'b t) -> 'b t
      val both : 'a t -> 'b t -> ('a * 'b) t
      val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
      val map3 : 'a t -> 'b t -> 'c t -> f:('a -> 'b -> 'c -> 'd) -> 'd t
      val map4 : 'a t -> 'b t -> 'c t -> 'd t -> f:('a -> 'b -> 'c -> 'd -> 'e) -> 'e t
    end
  end
end

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
    let ( <* ) p q = p >>= fun r -> (fun _ -> r) <$> q
  end

  module Let_syntax = struct
    module Let_syntax = struct
      let return = return
      let map m ~f = map m f
      let bind m ~f = bind m f
      let both = both
      let map2 m1 m2 ~f = map2 f m1 m2
      let map3 m1 m2 m3 ~f = map3 f m1 m2 m3
      let map4 m1 m2 m3 m4 ~f = map4 f m1 m2 m3 m4
    end
  end
end
