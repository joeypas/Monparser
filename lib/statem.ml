module type StateType = sig
  type t
end

module StateUpdate
    (M : Monad.S)
    (S : StateType)
    (F : sig
       val update : (S.t -> S.t) -> S.t M.t
     end) =
struct
  let update = F.update
  let set s = update (fun _ -> s)
  let fetch = update (fun x -> x)
end

module type S = sig
  type s
  type 'a m

  include Monad.S with type 'a t = s -> ('a * s) m

  val update : (s -> s) -> s t
  val set : s -> s t
  val fetch : s t
end

module Make (M : Monad.S) (T : StateType) : S with type s = T.t and type 'a m = 'a M.t =
struct
  type s = T.t
  type 'a m = 'a M.t

  module Inner = struct
    type 'a t = s -> ('a * s) m

    let return v = fun s -> M.return (v, s)
    let fail = fun _ -> M.fail
    let bind m f = fun s -> M.bind (m s) (fun (v, s') -> f v s')
    let map m f = fun s -> M.map (m s) (fun (x, s') -> f x, s')
    let plus p q = fun s -> M.plus (p s) (q s)
  end

  module StateM : Monad.S with type 'a t = s -> ('a * s) m = Monad.Make (Inner)
  include StateM

  include
    StateUpdate (StateM) (T)
      (struct
        let update f = fun s -> M.return (s, f s)
      end)
end
