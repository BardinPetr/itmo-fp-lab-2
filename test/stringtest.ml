module BagPropTests (B : Oabag.BAG with type elt = string) = struct
  open B

  let gen_ms =
    let open QCheck.Gen in
    string_printable |> list_size small_nat |> map of_list

  let gen_rep_list =
    let open QCheck.Gen in
    string_printable |> list_size small_nat

  let string_add_test =
    QCheck.Test.make ~count:1000 ~name:"Check string ms add"
      (QCheck.make gen_rep_list) (fun lst ->
        Utils.list_sort_compare lst
          (List.fold_right add lst (create 1) |> to_rep_seq |> List.of_seq))

  let string_rem_test =
    QCheck.Test.make ~count:1000 ~name:"Check string ms remove all"
      (QCheck.make gen_rep_list) (fun lst ->
        let src = List.fold_right add lst (create 1) in
        let out = List.fold_right remove lst src in
        out |> to_list |> List.is_empty && 0 = size out)

  let tests =
    [
      ( "String Tests",
        [ string_add_test; string_rem_test ]
        |> List.map QCheck_alcotest.to_alcotest );
    ]
end

module StringBag = Oabag.Make (String)
module StringTests = BagPropTests (StringBag)
