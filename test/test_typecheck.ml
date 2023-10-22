open OUnit2
open TalParser.Tal
open Typecheck

let tests = "test suite for typecheck" >::: [
  (* "stack equality 1" >:: (fun _ -> 
    let sty1 = Cons (Int, Cons (Int, Nil)) in
    let sty2 = Append (Cons (Int, Nil), Cons (Int, Nil)) in
    assert_bool "" (typecheck_stack_eq empty_env sty1 sty2 [])); *)
  "stack equality stkbeta4" >:: (fun _ -> 
    let sigma1 = Cons (Int, Nil) in
    let sigma2 = Cons (Int, Cons (Int, Nil)) in
    let sigma3 = Cons (TypeList [Int; Int], Nil) in
    let sty1 = Append(Append (sigma1, sigma2), sigma3) in
    let sty2 = Append(sigma1, Append(sigma2, sigma3)) in
    assert_bool "" (typecheck_stack_eq empty_env sty1 sty2 []));
  "stack equality 2" >:: (fun _ -> 
    ()
  )
]

let _ = run_test_tt_main tests