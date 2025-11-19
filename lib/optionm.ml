module S : Monad.S = struct
  module Inner : Monad.T with type 'a t = 'a option = struct
    type 'a t = 'a option

    let return v = Some v
    let fail = None

    let bind m f =
      match m with
      | Some x -> f x
      | None -> None
    ;;

    let map m f = Option.map f m

    let plus p q =
      match p with
      | Some x -> Some x
      | None -> q
    ;;
  end

  include Monad.Make (Inner)
end
