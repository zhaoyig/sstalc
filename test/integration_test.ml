open OUnit2
open Typecheck

let ic = open_in "testcases/tc_sst.stal"

let integration_tests = "test suite for typecheck" >::: [
  "tc_sst" >:: (fun _ ->
    let _ = typecheck "testcases/tc_sst.stal" in
    ()
  );
]

let _ = close_in ic

let _ = run_test_tt_main integration_tests
