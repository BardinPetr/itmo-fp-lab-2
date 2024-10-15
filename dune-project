(lang dune 3.16)

(name lab2)

(generate_opam_files true)

(source
 (github BardinPetr/itmo-fp-lab-2))

(authors "Bardin Petr")

(maintainers "Bardin Petr")

(license MIT)

(package
 (name lab2)
 (synopsis "ITMO FP course laboratory work #2")
 (description "Implemetation of OpenAddress Hashmap Bag")
 (depends ocaml dune (alcotest :with-test) (junit_alcotest :with-test) (qcheck :with-test) (qcheck-alcotest :with-test))
 (allow_empty))
