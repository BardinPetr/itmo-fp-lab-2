module BagPropTests (B : Oabag.BAG with type elt = int) = struct
  open B

  let gen_ms =
    let open QCheck.Gen in
    small_nat |> list_size small_nat |> map of_list

  let gen_3_ms = QCheck.Gen.triple gen_ms gen_ms gen_ms
  let create_empty_ms () = create 1000

  let monoid_identity =
    QCheck.Test.make ~count:100 ~name:"Monoid Identity element"
      (QCheck.make gen_ms) (fun ms ->
        let l = merge ms (create_empty_ms ()) in
        let r = merge (create_empty_ms ()) ms in
        equal l ms && equal r ms)

  let monoid_assoc =
    QCheck.Test.make ~count:100 ~name:"Monoid Associativity"
      (QCheck.make gen_3_ms) (fun (a, b, c) ->
        equal (merge (merge a b) c) (merge a (merge b c)))

  let tests =
    [
      ( "Property Tests",
        [ monoid_identity; monoid_assoc ]
        |> List.map QCheck_alcotest.to_alcotest );
    ]
end

module IntBag = Oabag.Make (Int)
module PropTests = BagPropTests (IntBag)
