module BagPropTests (B : Oabag.BAG with type elt = int) = struct
  open B

  let gen_ms =
    let open QCheck.Gen in
    small_nat |> list_size (int_range 1 50) |> map of_list

  let empty_ms () = create 100

  let monoid_identity_elem =
    QCheck.Test.make ~count:100 ~name:"Monoid Identity element"
      (QCheck.make gen_ms) (fun _ -> true)

  let prop_tests = [ monoid_identity_elem ]

  let tests =
    [ ("Property Tests", prop_tests |> List.map QCheck_alcotest.to_alcotest) ]
end

module IntBag = Oabag.Make (Int)
module PropTests = BagPropTests (IntBag)
module IntSet = Set.Make (Int)
