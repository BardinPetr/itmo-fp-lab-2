module BagPropTests (B : Oabag.BAG with type elt = int) = struct
  open B

  let gen_ms =
    let open QCheck.Gen in
    small_nat |> list_size small_nat |> map of_list

  let create_empty_ms () = create 1000

  let monoid_identity =
    QCheck.Test.make ~count:100 ~name:"Monoid Identity element"
      (QCheck.make gen_ms) (fun ms ->
        let l = merge ms (create_empty_ms ()) in
        let r = merge (create_empty_ms ()) ms in
        equal l ms && equal r ms)

  let gen_3_ms = QCheck.Gen.triple gen_ms gen_ms gen_ms

  let monoid_assoc =
    QCheck.Test.make ~count:100 ~name:"Monoid Associativity"
      (QCheck.make gen_3_ms) (fun (a, b, c) ->
        equal (merge (merge a b) c) (merge a (merge b c)))

  (** generate list with at least one repeated element *)
  let gen_rep_list =
    let open QCheck.Gen in
    int_range 1 10 |> list_size (int_range 11 100)

  (** check that set iteratively built from list with repetitions contains same
      elements with same multipkicities*)
  let add_count =
    QCheck.Test.make ~count:1000
      ~name:"Preserves duplicate elements on additions"
      (QCheck.make gen_rep_list) (fun lst ->
        Utils.list_sort_compare lst
          (List.fold_right add lst (create 1) |> to_rep_seq |> List.of_seq))

  let gen_ms_and_list = QCheck.Gen.pair gen_ms gen_rep_list

  (** check that any set would not change after applying add operation to it*)
  let set_immutability =
    QCheck.Test.make ~count:100 ~name:"Set is immutable over add operation"
      (QCheck.make gen_ms_and_list) (fun (ms, to_add) ->
        let orig = copy ms in
        let _ = List.fold_right add to_add ms in
        equal orig ms)

  let tests =
    [
      ( "Property Tests",
        [ monoid_identity; monoid_assoc; add_count; set_immutability ]
        |> List.map QCheck_alcotest.to_alcotest );
    ]
end

module IntBag = Oabag.Make (Int)
module PropTests = BagPropTests (IntBag)
