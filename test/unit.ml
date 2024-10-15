open Alcotest

module BagTests (B : Oabag.BAG with type elt = int) = struct
  open B

  let demo_list sz rep =
    Seq.(ints 1 |> take sz |> cycle |> take (rep * sz)) |> List.of_seq

  let list_check = check (slist int Stdlib.compare) "Valid list"

  let pair_list_check =
    check (slist (pair int int) Stdlib.compare) "Valid pair list"

  let test_add () =
    let m = create 10 |> add 1 |> add 2 |> add 3 |> add 4 |> to_list in
    pair_list_check [ (1, 1); (2, 1); (3, 1); (4, 1) ] m

  let test_add_dup () =
    let test_seq = demo_list 5 4 in
    let m = List.fold_right add test_seq (create 10) in
    pair_list_check
      (Seq.(ints 1 |> take 5 |> map (fun i -> (i, 4))) |> List.of_seq)
      (m |> to_list)

  let test_add_rem () =
    let lst = demo_list 5 5 in
    let m = of_list lst in
    let m_rem = List.fold_right remove (demo_list 3 2) m in
    pair_list_check
      [ (1, 3); (2, 3); (3, 3); (4, 5); (5, 5) ]
      (*remove 2 times '1' '2' and '3'*)
      (m_rem |> to_list)

  let test_grow () =
    let target = demo_list 100 2 |> of_list in
    let to_check = List.fold_right add (demo_list 100 2) (create 1) in
    pair_list_check (target |> to_list) (to_check |> to_list)

  let test_join () =
    let a = demo_list 3 3 |> of_list in
    let b = demo_list 3 3 |> List.map (fun i -> i + 3) |> of_list in
    let res = a |> merge b in
    let valid = demo_list 6 3 |> of_list in
    pair_list_check (valid |> to_list) (res |> to_list)

  let test_from_list () =
    let m = of_list [ 1; 2; 3; 3 ] |> to_list in
    pair_list_check [ (1, 1); (2, 1); (3, 2) ] m

  let test_to_list () =
    let lst = demo_list 5 5 in
    let m = of_list lst |> to_rep_seq |> List.of_seq in
    list_check m lst

  let test_size () =
    let m = of_list [ 1; 2; 2; 3; 3; 3 ] in
    check int "Total size" 6 (m |> total);
    check int "Distinct size" 3 (m |> size)

  let test_count () =
    let m = of_list [ 11; 22; 22; 33; 33; 33 ] in
    check (list int) "Count of exitsing element" [ 1; 2; 3 ]
      ([ 11; 22; 33 ] |> List.map (fun i -> m |> count i));
    check int "Count not existing" 0 (m |> count 404)

  let test_equals () =
    let m1 = of_list [ 1; 2; 3; 4; 4 ] in
    let m2 = of_list [ 1; 2; 4 ] |> add 3 |> add 4 in
    let m3 = of_list [ 1; 2; 4 ] in
    check bool "Equal() of equal sets" true (equal m1 m2);
    check bool "Equal() of not equal sets" false (equal m1 m3)

  let test_fold () =
    let res =
      demo_list 5 2 |> of_list |> fold (fun acc (v, c) -> acc + (v * c)) 1000
    in
    check int "Fold sum value" (1000 + (2 * (1 + 2 + 3 + 4 + 5))) res

  let test_filter () =
    let m = of_list [ 1; 2; 2; 3; 3; 3; 4; 4; 4; 4 ] in
    let f = m |> filter (fun (v, c) -> v > 2 && c < 4) in
    pair_list_check [ (3, 3) ] (f |> to_list)

  let test_map () =
    let m = demo_list 5 2 |> of_list |> map (fun v -> v * 10) |> to_list in
    pair_list_check (demo_list 5 1 |> List.map (fun i -> (i * 10, 2))) m

  let test_mapc () =
    let m =
      demo_list 5 2 |> of_list |> mapc (fun (v, _) -> (v * 2, v * 3)) |> to_list
    in
    pair_list_check (demo_list 5 1 |> List.map (fun i -> (i * 2, i * 3))) m

  let tests =
    [
      ( "Add/Remove",
        [
          test_case "Add" `Quick test_add;
          test_case "Add with duplicates" `Quick test_add_dup;
          test_case "Add & Remove" `Quick test_add_rem;
          test_case "Join" `Quick test_join;
          test_case "Growth" `Quick test_grow;
        ] );
      ( "Utils",
        [
          test_case "From List" `Quick test_from_list;
          test_case "As List" `Quick test_to_list;
          test_case "Size/Distinct" `Quick test_size;
          test_case "Count" `Quick test_count;
          test_case "Equals" `Quick test_equals;
        ] );
      ( "Operations",
        [
          test_case "Fold" `Quick test_fold;
          test_case "Filter" `Quick test_filter;
          test_case "Map" `Quick test_map;
          test_case "Map with new counts" `Quick test_mapc;
        ] );
    ]
end

module IntBag = Oabag.Make (Int)
module UnitTests = BagTests (IntBag)
