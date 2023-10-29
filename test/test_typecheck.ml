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
    typecheck_stack_eq empty_env sty1 sty2; );
  "stack equality 2" >:: (fun _ -> 
    ()
  );
  "free var type 1" >:: (fun _ -> 
    let ta = [TAISTVar (STVar "a"); TAITVar (TVar "b")] in
    let ra = ((StackTypeVar (STVar "a")), [(Rax, Int); (Rbx, (Var (TVar "b"))); (Rcx, (Var (TVar "c")))]) in
    let fv = free_vars_ty (Forall (ta, ra)) in
    assert_equal fv [TAITVar (TVar "c")]
  );
  "free var type 2" >:: (fun _ -> (* Variables bound in Forall type and Exist type not considered free *)
    let ta = [TAISTVar (STVar "a"); TAITVar (TVar "b")] in
    let ra = ((StackTypeVar (STVar "a")), [(Rax, Int); (Rbx, (Var (TVar "b"))); (Rcx, (Var (TVar "c")))]) in
    let fv = free_vars_ty (TypeList [Forall (ta, ra); (Var (TVar "d")); Exist ((TVar "exist_var"), TypeList [(Var (TVar "exist_var")); (Var (TVar "e"))])]) in
    assert_equal fv [TAITVar (TVar "c"); TAITVar (TVar "d"); TAITVar (TVar "e")]
  );
  "substitution type" >:: (fun _ -> 
    let tao1 = TypeList [Int; TypeList [Int; Int]] in
    assert_equal (ty_substitute tao1 Int (TVar "alpha")) (TypeList [(Var (TVar "alpha")); TypeList [(Var (TVar "alpha")); (Var (TVar "alpha"))]])
  );
  "serialize" >:: (fun _ -> 
    let sty = (Int ++ StackTypeVar (STVar "rho1")) @@ (Int ++ (Int ++ Nil)) in
    let expect = [SITy Int; SISty (StackTypeVar (STVar "rho1")); SITy Int; SITy Int] in
    assert_equal (serialize_sty sty) expect
  );
  "diff" >:: (fun _ ->
    assert_equal (diff [1] [1]) []
  );
  "insert into stack" >:: (fun _ ->
    let sigma = TTop ++ (TTop ++ Nil) in
    assert_equal (deserialize_sty (insert_ty_to_sit Int (serialize_sty sigma) 0)) (Int ++ (TTop ++ Nil))
  )
]

let _ = run_test_tt_main tests