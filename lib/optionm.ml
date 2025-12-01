module S : Monad.S with type 'a t = 'a option = struct
  module Inner = struct
    type 'a t = 'a option

    let return v = Some v
    let fail = None

    let bind m f =
      match m with
      | Some x -> f x
      | None -> None
    ;;

    let map m f =
      match m with
      | Some x -> Some (f x)
      | None -> None
    ;;

    let plus p q =
      match p with
      | Some x -> Some x
      | None -> q
    ;;
  end

  include Monad.Make (Inner)
end
