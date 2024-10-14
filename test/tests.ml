(*let report_path = Sys.getenv_opt "REPORT_PATH"

  let () =
     Utils.run_with_save_report "euler" "Euler problems solutions test" report_path
       [] *)

(* let im = Oabag.(create 10) *)
(* open Multiset module *)

(* open Alcotest *)
(* open Oabag *)

(* let test_add () =
     let m = create 10 |> add "test" in
     (check int) "Size after adding one element" 1 (size m);
     (check int) "Total size after adding one element" 1 (total_size m);
     (check int) "Count of added element" 1 (count m "test")


   let test_add_duplicate () =
     let m = create 10 |> add "test" |> add "test" in
     (check int "Size after adding duplicate element" 1 (size m);
     (check int) "Total size after adding duplicate element" 2 (total_size m);
     (check int) "Count of added element" 2 (count m "test")


   let () =
     let open Alcotest in
     run "Multiset Tests" [
       "Basic Operations", [
         test_case "Creation" `Quick test_create;
         test_case "Add Element" `Quick test_add;
         test_case "Add Duplicate Element" `Quick test_add_duplicate;
       ]
     ] *)

(* open Oabag *)

(* module type INT_BAG = Oabag with type t = int *)

module IntBag = Oabag.Make (Int)

(* let m =
   let open IntBag in
   create 100 |> add 31 |> add 2342 |> add 123 |> to_seq *)
