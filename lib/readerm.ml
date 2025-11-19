module type E = sig
  type t
end

module type S = sig
  include Monad.S

  type e
  type s

  val getenv : e t
  val setenv : e -> 'a t -> 'a t
  val update : (s -> s) -> s t
  val set : s -> s t
  val fetch : s t
end

module Make (S : Statem.S) (E : E) :
  S with type 'a t = E.t -> 'a S.t and type e = E.t and type s = S.s = struct
  type e = E.t
  type s = S.s

  module Inner : Monad.T with type 'a t = e -> 'a S.t = struct
    type 'a t = e -> 'a S.t

    let return v = fun _ -> S.return v
    let fail = fun _ -> S.fail
    let bind m f = fun s -> S.bind (m s) (fun v -> f v s)
    let map m f = bind m (fun a -> return (f a))
    let plus p q = fun s -> S.plus (p s) (q s)
  end

  module ReaderM : Monad.S with type 'a t = e -> 'a S.t = Monad.Make (Inner)
  include ReaderM

  let getenv : e t = fun s -> S.return s
  let setenv s m = fun _ -> m s

  include
    Statem.StateUpdate
      (ReaderM)
      (struct
        type t = S.s
      end)
      (struct
        let update f = fun _ -> S.update f
      end)
end
