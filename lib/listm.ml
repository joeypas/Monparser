module S : Monad.S with type 'a t = 'a list = struct
  module Inner = struct
    type 'a t = 'a list

    let return v = [ v ]
    let fail = []
    let bind m f = List.concat (List.map f m)
    let map m f = List.map f m

    let plus p q =
      match p with
      | [] -> q
      | x :: xs -> x :: (xs @ q)
    ;;
  end

  include Monad.Make (Inner)
end
