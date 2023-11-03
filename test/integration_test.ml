open OUnit2
open Typecheck

let ic1 = open_in "testcases/tc_sst.stal"
let ic2 = open_in "testcases/factorial.stal"

let integration_tests = "test suite for typecheck" >::: [
  "tc_sst" >:: (fun _ ->
    let _ = typecheck "testcases/tc_sst.stal" in
    ()
  );
  "factorial" >:: (fun _ ->
    let _ = typecheck "testcases/factorial.stal" in
    ()
  )
]

let _ = close_in ic1
let _ = close_in ic2

let _ = run_test_tt_main integration_tests
