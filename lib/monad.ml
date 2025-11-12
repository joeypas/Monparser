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
  include M

  let ( >>= ) = bind
  let ( >>| ) = map
  let ( <+> ) = plus
  let ( <$> ) g m = map m g
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

module type State = sig
  type t
end

module StateUpdate
    (M : MonadPlus)
    (S : State)
    (F : sig
       val update : (S.t -> S.t) -> S.t M.t
     end) =
struct
  let update = F.update
  let set s = update (fun _ -> s)
  let fetch = update (fun x -> x)
end

module type StateMonad = sig
  type s
  type 'a m

  module S : State with type t = s
  module StateM : MonadPlus with type 'a t = s -> ('a * s) m

  val update : (s -> s) -> s StateM.t
  val set : s -> s StateM.t
  val fetch : s StateM.t
end

module StateMonad = struct
  module Make (M : MonadPlus) (S : State) = struct
    module StateM : MonadPlus with type 'a t = S.t -> ('a * S.t) M.t = struct
      type 'a t = S.t -> ('a * S.t) M.t

      let return v = fun s -> M.return (v, s)
      let fail = fun _ -> M.fail
      let bind m f = fun s -> M.bind (m s) (fun (v, s') -> f v s')
      let map m g = fun s -> M.map (m s) (fun (v, s') -> g v, s')
      let plus p q = fun s -> M.plus (p s) (q s)
    end

    include MonadPlusInfix (StateM)

    include
      StateUpdate (StateM) (S)
        (struct
          let update f = fun s -> M.return (s, f s)
        end)
  end

  module Lift (M : MonadPlus) (S : State) :
    StateMonad with type 'a m = 'a M.t and type s = S.t = struct
    type s = S.t
    type 'a m = 'a M.t

    module S = S
    include Make (M) (S)
  end
end

module ReaderMonad = struct
  module Make (ST : StateMonad) (Env : State) = struct
    module ReaderM : MonadPlus with type 'a t = Env.t -> 'a ST.StateM.t = struct
      open ST

      type 'a t = Env.t -> 'a StateM.t

      let return v = fun _ -> StateM.return v
      let bind m f = fun s -> StateM.bind (m s) (fun v -> f v s)
      let map m g = fun s -> StateM.map (m s) (fun v -> g v)
      let fail = fun _ -> StateM.fail
      let plus p q = fun s -> StateM.plus (p s) (q s)
    end

    include MonadPlusInfix (ReaderM)

    let getenv : Env.t t = fun s -> ST.StateM.return s
    let setenv (s : Env.t) (m : 'a t) : 'a t = fun _ -> m s

    include
      StateUpdate (ReaderM) (ST.S)
        (struct
          let update f = fun _ -> ST.update f
        end)
  end
end
