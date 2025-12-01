module type EnvType = sig
  type t
end

module type S = sig
  type env
  type s
  type 'a m

  include Monad.S with type 'a t = env -> 'a m

  val getenv : env t
  val setenv : env -> 'a t -> 'a t
  val update : (s -> s) -> s t
  val set : s -> s t
  val fetch : s t
end

module Make (S : Statem.S) (E : EnvType) :
  S with type 'a m = 'a S.t and type env = E.t and type s = S.s = struct
  type env = E.t
  type s = S.s
  type 'a m = 'a S.t

  module Inner = struct
    type 'a t = env -> 'a S.t

    let return v = fun _ -> S.return v
    let fail = fun _ -> S.fail
    let bind m f = fun s -> S.bind (m s) (fun v -> f v s)
    let map m f = bind m (fun a -> return (f a))
    let plus p q = fun s -> S.plus (p s) (q s)
  end

  module ReaderM : Monad.S with type 'a t = env -> 'a S.t = Monad.Make (Inner)
  include ReaderM

  let getenv : env t = fun s -> S.return s
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
