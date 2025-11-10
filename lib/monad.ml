(*
   This is our base monad type
   (Monad and MonadOfPlus combined)
*)
module type MonadPlus = sig
  type 'a t

  val return : 'a -> 'a t
  val fail : 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val map : 'a t -> ('a -> 'b) -> 'b t
  val plus : 'a t -> 'a t -> 'a t
end

(*
   Functor to add infix operators to a Monad
*)
module MonadPlusInfix (M : MonadPlus) = struct
  let ( >>= ) = M.bind
  let ( >>| ) = M.map
  let ( <+> ) = M.plus
  let ( <$> ) g m = M.map m g
  let seq p q = p >>= fun f -> q >>| f
  let ( <*> ) = seq
  let ( *> ) p q = p >>= fun _ -> q
  let ( <* ) p q = p >>= fun r -> (fun _ -> r) <$> q
end

(*
   Non-Determinism Monad
*)
module ListM : MonadPlus with type 'a t = 'a list = struct
  type 'a t = 'a list

  let return v = [ v ]
  let fail = []

  let rec bind m f =
    match m with
    | [] -> []
    | x :: xs -> f x @ bind xs f
  ;;

  let map m f =
    let res = bind m (fun a -> return (f a)) in
    res
  ;;

  let plus p q =
    match p with
    | [] -> q
    | x :: xs -> x :: (xs @ q)
  ;;
end

(*
   Determinism Monad
*)
module MaybeM : MonadPlus with type 'a t = 'a option = struct
  type 'a t = 'a option

  let return v = Some v
  let fail = None

  let bind m f =
    match m with
    | Some x -> f x
    | None -> None
  ;;

  let map m f =
    let res = bind m (fun a -> return (f a)) in
    res
  ;;

  let plus p q =
    match p with
    | Some x -> Some x
    | None -> q
  ;;
end

module MakeState
    (M : MonadPlus)
    (S : sig
       type t
     end) =
struct
  type s = S.t

  module StateM : MonadPlus with type 'a t = s -> ('a * s) M.t = struct
    type 'a t = s -> ('a * s) M.t

    let return v : 'a t = fun s -> M.return (v, s)
    let fail : 'a t = fun _ -> M.fail

    let bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
      fun s -> M.bind (m s) (fun (v, s') -> f v s')
    ;;

    let map (m : 'a t) (g : 'a -> 'b) : 'b t =
      fun s -> M.map (m s) (fun (v, s') -> g v, s')
    ;;

    let plus (p : 'a t) (q : 'a t) : 'a t = fun s -> M.plus (p s) (q s)
  end

  include MonadPlusInfix (StateM)
  include StateM

  let update (f : s -> s) : 'a t =
    fun s ->
    M.return
      ( s
      , try f s with
        | _ -> s )
  ;;

  let set s = update (fun _ -> s)
  let fetch = update (fun x -> x)
end

module MakeReader
    (M : MonadPlus)
    (S1 : sig
       type t
     end)
    (S2 : sig
       type t
     end) =
struct
  module SM = MakeState (M) (S1)

  type s = S2.t

  module ReaderM : MonadPlus with type 'a t = s -> 'a SM.t = struct
    type 'a t = s -> 'a SM.t

    let return v = fun _ -> SM.return v
    let bind m f = fun s -> SM.bind (m s) (fun v -> f v s)
    let map m g = fun s -> SM.map (m s) (fun v -> g v)
    let fail = fun _ -> SM.fail
    let plus p q = fun s -> SM.plus (p s) (q s)
  end

  include MonadPlusInfix (ReaderM)
  include ReaderM

  let env : 'a t = fun s -> SM.return s
  let setenv s m = fun _ -> m s
  let update (f : S1.t -> S1.t) : S1.t t = fun _ -> SM.update f
  let set s = update (fun _ -> s)
  let fetch = update (fun x -> x)
end
