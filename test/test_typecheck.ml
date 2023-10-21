open OUnit2

let tests = "test suite for typecheck" >::: [
  "empty" >:: (fun _ -> assert_equal 0 1)
]

let _ = run_test_tt_main tests