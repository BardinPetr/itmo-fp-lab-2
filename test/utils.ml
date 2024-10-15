(** [run_with_save_report] runs is a wrapper of wrapper of Alcotest.run which
    automatically creates JUnit XML test report *)
let run_with_save_report package_name test_name report_file tests =
  let output, _ =
    Junit_alcotest.run_and_report ~package:package_name test_name tests
  in
  match report_file with
  | Some path -> ignore (([ output ] |> Junit.make |> Junit.to_file) path)
  | None -> ()

let list_sort_compare a b =
  0
  = List.compare Stdlib.compare
      (List.sort Stdlib.compare a)
      (List.sort Stdlib.compare b)
