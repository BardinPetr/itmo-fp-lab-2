open Oabag
module IntBag = Make (Int)
module StringBag = Make (String)
module ISMapper = MakeMapper (IntBag) (StringBag)
module SIMapper = MakeMapper (StringBag) (IntBag)

let gen_int_list =
  let open QCheck.Gen in
  int_range 10000 99999 |> list_size (int_range 100 1000)

let f_is = string_of_int
let f_si = int_of_string
let f_is_mapper (v, c) = (f_is v, c)
let f_si_mapper (v, c) = (f_si v, c)

let check_map_isi =
  QCheck.Test.make ~count:1000 ~name:"Cycle map int->str->int"
    (QCheck.make gen_int_list) (fun src_list ->
      let src_int_ms = src_list |> IntBag.of_list in
      let out_str_ms = ISMapper.map f_is_mapper src_int_ms in
      let out_int_ms = SIMapper.map f_si_mapper out_str_ms in
      IntBag.equal src_int_ms out_int_ms)

let check_map_sis =
  QCheck.Test.make ~count:1000 ~name:"Cycle map str->int->str"
    (QCheck.make gen_int_list) (fun src_list ->
      let src_str_ms = src_list |> List.map f_is |> StringBag.of_list in
      let out_int_ms = SIMapper.map f_si_mapper src_str_ms in
      let out_str_ms = ISMapper.map f_is_mapper out_int_ms in
      StringBag.equal src_str_ms out_str_ms)

let tests =
  [
    ( "Two-type Map Tests",
      [ check_map_isi; check_map_sis ] |> List.map QCheck_alcotest.to_alcotest
    );
  ]
