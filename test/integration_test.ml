open OUnit2
open Typecheck

let file_list = [
  "testcases/tc_sst.stal";
  "testcases/factorial.stal";
  "testcases/infloop.stal";
  "testcases/newstack.stal"
]

let ic_list = List.map (fun x -> open_in x) file_list

let integration_tests = "test suite for typecheck" >::: [
  "tc_sst" >:: (fun _ ->
    let _ = typecheck "testcases/tc_sst.stal" in
    ()
  );
  "factorial" >:: (fun _ ->
    let _ = typecheck "testcases/factorial.stal" in
    ()
  );
  "inf loop" >:: (fun _ ->
    let _ = typecheck "testcases/infloop.stal" in
    ()
  );
  "new stack" >:: (fun _ ->
    let _ = typecheck "testcases/newstack.stal" in
    ()
  );
]

let _ = List.map (fun x -> close_in x) ic_list

let _ = run_test_tt_main integration_tests
