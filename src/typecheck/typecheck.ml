open TalParser.Main
open TalParser.Tal
open Prettyprint
open Utils
exception TypeError of string
exception StackTypeError of string
open Printf

let type_error s = 
  raise (TypeError s)

type static_env = label_asgn * reg_asgn * ty_asgn * access_cap * alloc_cap

let empty_env = ([], [], [], CapNil, ACapNil)

let get_la env = let (l, _, _, _, _) = env in l

let get_ra env = let (_, r, _, _, _) = env in r

let get_ta env = let (_, _, t, _, _) = env in t

let get_access_cap env = let (_, _, _, cap, _) = env in cap

let get_alloc_cap env = let (_, _, _, _, cap) = env in cap

let set_la env la' = 
  let (_, r, t, acc_cap, all_cap) = env in (la', r, t, acc_cap, all_cap)

let set_ra env ra' = 
  let (l, _, t, acc_cap, all_cap) = env in (l, ra', t, acc_cap, all_cap)

let set_ta env ta' = 
  let (l, r, _, acc_cap, all_cap) = env in (l, r, ta', acc_cap, all_cap)

let set_access_cap env access_cap' = 
  let (l, r, t, _, all_cap) = env in (l, r, t, access_cap', all_cap)

let set_alloc_cap env alloc_cap' = 
  let (l, r, t, acc_cap, _) = env in (l, r, t, acc_cap, alloc_cap')

let insert_con_var env (con_var : con_var) =
  let ta = get_ta env in
  let ta' = (CtxtConVar con_var) :: ta in
  set_ta env ta'

let insert_loc env (loc_var : loc_var) (loc : location) =
  let ta = get_ta env in
  let ta' = (CtxtLoc (loc_var, loc)) :: ta in
  set_ta env ta'
 
(****************************** Helper functions ******************************)
(* Return a string about a type mismatch error *)
let type_err_expect str expected_ty actual_ty =
  Printf.sprintf "Expect %s to have type %s, but got %s\n" str (pp_ty expected_ty) (pp_ty actual_ty)

(* Returns the `code` corresponding to label `l`, in the code_block_seq `ast` *)
let rec lookup_code_block ast l =
  match ast with
  | CodeBlockSeq code_block -> 
    (match code_block with
      | CodeBlock (label, code) -> 
        if label = l then code
        else failwith ("Cannot find label" ^ l))
  | CodeBlockSeqCons (h, t) ->
    (match h with
      | CodeBlock (label, code) -> 
        if label = l then code
        else lookup_code_block t l)

let lookup_label (env: static_env) (x : string) : ty =
  let label_assignment = get_la env in
  match List.assoc_opt x label_assignment with
  | Some t -> t
  | None -> failwith ("Unbound label: " ^ x)

(* TODO: Avoid duplication? *)
let extend_label label_assignment label_name typ =
  (label_name, typ) :: label_assignment

let lookup_register (env : static_env) (r : reg) : ty =
  let ra = get_ra env in
  match (List.assoc_opt r ra) with
  | Some t -> t
  | None -> failwith ("Unbound register: " ^ (pp_reg r))

(* Return a new register assignment with `reg` having type `t` *)
let update_register_asgn (ra : reg_asgn) (r : reg) (t : ty) =
  update_assoc_list r t ra

(* All the elements in l1 that is not in l2 *)
let diff l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1

(* free vars of a register asgn *)
(* let rec free_vars_ra (ra : reg_asgn) =
  let (sty, normal_reg) = ra in
  (free_vars_sty sty) @ (List.concat_map (fun x -> let (_, t) = x in free_vars_ty t) normal_reg) *)

(* and free_vars_sty sty = 
  match sty with
  | StackTypeVar v -> [TAISTVar v]
  | Nil -> []
  | Cons (typ, sty) -> (free_vars_ty typ) @ (free_vars_sty sty)
  | Append (sty1, sty2) -> (free_vars_sty sty1) @ (free_vars_sty sty2) *)

(* return the free variables in a `ty` as a list *)
(* and free_vars_ty (typ : ty) =
  match typ with
  | Var v -> [TAITVar v] (* Type variable *)
  | Int -> []
  | TypeList l -> List.concat_map free_vars_ty l
  | Forall (ta, ra, _) -> 
    let l1 = free_vars_ra ra in
    let l2 = ta in
    diff l1 l2
  | Exist (_, _, _) -> [] (* TODO *)
  | TPtr sty -> free_vars_sty sty
  | TTop -> []
  | _ -> failwith "TODO" *)

(* To make life easy *)
(* let (@@) sty1 sty2 = Append (sty1, sty2)
let (++) ty sty = Cons (ty, sty) *)


(* Serialize a stack_ty into a list of stack_item (either stack type variable or ty) *)
(* let rec serialize_sty sty = 
  match sty with
  | Cons (ty, sty') -> (SITy ty) :: (serialize_sty sty')
  | Append (sty1, sty2) -> (serialize_sty sty1) @ (serialize_sty sty2)
  | Nil -> []
  | StackTypeVar _ -> [SISty sty] *)

(*  *)
let rec strip_n_types l n =
  if n = 0 then l else
  match l with
  | [] -> failwith "cannot sfree because stack does not have enough items to free."
  | h :: t -> (match h with
    | SITy _ -> strip_n_types t (n - 1)
    | SISty _ -> failwith "cannot sfree because stack does not have enough items to free.")

    (* if List.length acc >= n then (acc, sigma) else
  match sigma with
  | Cons (tao, sigma') ->
    serialize_first_n sigma' (tao :: acc) n
  | Append (sigma1, sigma2) ->
    let (l1, _) = serialize_first_n sigma1 acc n in
    let l2 = serialize_first_n sigma2 l1 n in
    l2
  | Nil -> ([], sigma)
  | StackTypeVar _ -> ([], sigma) *)

(* sit means stack item list, this insert also erases the item in there originally *)
let rec insert_ty_to_sit ty sit i = 
  if i = 0 then
    (match sit with
    | [] -> failwith "cannot sst because stack is not big enough"
    | _ :: t -> (SITy ty) :: t)
  else
  match sit with
  | [] -> failwith "cannot sst because stack is not big enough"
  | h :: t -> h :: (insert_ty_to_sit ty t (i - 1))


(* let rec deserialize_sty l =
  match l with
  | h :: t ->
    (match h with
    | SITy tao -> Cons (tao, deserialize_sty t)
    | SISty rho -> 
      (match t with
      | _ :: _ -> Append (rho, deserialize_sty t)
      | [] -> rho))
  | [] -> Nil *)

let update_sp env sp' = 
  let (l, r, t) = env in
  let (_, normal_reg) = r in
  (l, (sp', normal_reg), t)

let get_stack_type env =
  let (_, r, _) = env in
  let (sp, _) = r in
  sp

(* when sigma1 = sigma2 @ sigma3, this function finds out sigma2. If it doesn't exist then throw error *)
let rec head_of_sit sit1 sit3 acc = 
  if sit1 = sit3 then acc else
  match sit1 with
  | [] -> raise (StackTypeError "")
  | h :: t -> head_of_sit t sit3 (acc @ [h])

(* takes an env and return new env *)
let update_register (env : static_env) reg typ =
  let (l, r, t, cap, acap) = env in
  let r' = update_register_asgn r reg typ in
  (l, r', t, cap, acap)

(* delete the register assignment of reg from env *)
let delete_register (env : static_env) reg =
  let (l, ra, t, access_cap, alloc_cap) = env in
  let ra' = List.filter (fun x -> let (reg', _) = x in reg' <> reg) ra in
  (l, ra', t, access_cap, alloc_cap)

(* get the ith type in a stack item list, raise type error if theres a stack variable in the first i item *)
let rec get_ith_type (l : stack_item list) i =
  match l with
  | [] -> type_error "stack is not big enough"
  | h :: t -> (match h with
    | SITy tao -> 
      if i = 0 then tao
      else get_ith_type t (i - 1)
    | SISty _ ->
      type_error "encountered stack variable in stack while getting a type from it"
  )

(* let typecheck_ty env typ =
  let free_vars = free_vars_ty typ in
  let env_vars = get_ta env in
  let unbound_free_vars = diff free_vars env_vars in
  if unbound_free_vars <> [] then
    type_error ((pp_ty_asgn unbound_free_vars) ^ " is unbound") *)

(* stype *)
(* let rec typecheck_sty env sty =
  let free_vars = free_vars_sty sty in
  let (_, _, env_vars) = env in
  let unbound_free_vars = diff free_vars env_vars in
  if unbound_free_vars <> [] then
    type_error ((pp_ty_asgn unbound_free_vars) ^ " is unbound in stack type " ^ (pp_sty sty)) *)

(* ============================ Well-formedness ============================ *)
let in_context (ctxt : ty_asgn) (cv : con_var) =
  match List.mem (CtxtConVar cv) ctxt with
  | true -> ()
  | false -> type_error "" (* TODO: *)

let in_context_bool (ctxt : ty_asgn) (cv : con_var) =
  List.mem (CtxtConVar cv) ctxt

let rec wf_loc (env : static_env) (loc : location) =
  match loc with
  | LocCon lv -> 
    let ta = get_ta env in
    in_context ta (CVLoc lv)
  | LocB -> ()
  | LocNext loc' -> wf_loc env loc'

let concat_ctxt ctxt1 ctxt2 =
  ctxt1 @ ctxt2

let rec wf_ctxt env ctxt =
  let ta = get_ta env in
  match ctxt with
  | [] -> ()
  | h :: t ->
    (match h with
    | CtxtConVar _ -> wf_ctxt env t
    | CtxtLoc (_, loc) -> 
      let _ = wf_ctxt env t in
      let env' = set_ta env (concat_ctxt ctxt ta) in
      let _ = wf_loc env' loc in
      ())

let rec wf_ty (env : static_env) (typ : ty) =
  match typ with
  | TyCon tv ->
    in_context (get_ta env) (CVTy tv)
  | TTop -> ()
  | Int -> ()
  | TyPtr l -> wf_loc env l
  | Forall (delta', ra, access_cap, alloc_cap) ->
    let _ = wf_ctxt env delta' in (* 1st *)
    let delta = get_ta env in 
    let env' = set_ta env (concat_ctxt delta delta') in
    let _ = wf_ty_regfile env' ra in (* 2nd *)
    let _ = wf_cap env' access_cap in (* 3rd *)
    let _ = wf_acap env' alloc_cap in (* 4th *)
    ()
  | _ -> failwith "TODO"

(* Iterate through normal registers and typecheck each type *)
and wf_ty_regfile env (l : reg_asgn) =  
  match l with
  | [] -> ()
  | h :: t -> 
    let (_, typ) = h in
    let _ = wf_ty env typ in
    wf_ty_regfile env t

and wf_cap env access_cap =
  match access_cap with
  | CapCon cv ->
    in_context (get_ta env) (CVCap cv)
  | CapNil ->
    ()
  | CapTy (l, tao) ->
    let _ = wf_loc env l in
    wf_ty env tao
  | CapATy (l, sigma) ->
    let _ = wf_loc env l in
    wf_aty env sigma
  | CapOTimes (c1, c2) ->
    let _ = wf_cap env c1 in
    wf_cap env c2
  | CapWedge (c1, c2) ->
    let _ = wf_cap env c1 in
    wf_cap env c2
  | CapFrac (c1, c2) ->
    let _ = wf_cap env c1 in
    wf_cap env c2
  
and wf_aty env aty =
  match aty with
  | ATyNil -> ()
  | ATyCon atyv -> in_context (get_ta env) (CVATy atyv)
  | ATyCons (tao, sigma) -> let _ = wf_aty env sigma in wf_ty env tao

and wf_acap env alloc_cap = (* TODO: Is this right? *)
  match alloc_cap with
  | ACapNil -> ()
  | ACapCon acap_var -> in_context (get_ta env) (CVACap acap_var)
  | ACap (location, _) ->
    wf_loc env location
  | ACapCons (acap1, acap2) ->
    let _ = wf_acap env acap1 in
    wf_acap env acap2

let kind_of_con env c =
  match c with
  | ConLoc loc -> let _ = wf_loc env loc in KLoc
  | ConCap cap -> let _ = wf_cap env cap in KCap
  | ConACap acap -> let _ = wf_acap env acap in KACap
  | ConATy atyp -> let _ = wf_aty env atyp in KATy
  | ConTy typ -> let _ = wf_ty env typ in KTy

let typeof_reg env reg =
  lookup_register env reg

let kind_of_con_var (cv : con_var) =
  match cv with
  | CVLoc _ -> KLoc
  | CVCap _ -> KCap
  | CVACap _ -> KACap
  | CVTy _ -> KTy
  | CVATy _ -> KATy

let rec typeof_operand env op =
  match op with
  | Reg reg -> typeof_reg env reg (* reg *)
  | Word word_val -> typeof_word env word_val 
  | OperandIns (op, c) -> 
    let op_type = typeof_operand env op in
    (match op_type with
    | Forall (ta, ra, access_cap, alloc_cap) ->
      (match ta with
      | [] -> type_error ("The type assignments of " ^ (pp_op op) ^ " is empty.")
      | h :: tail_ctxt ->
        (match c with
          | ConLoc loc -> (* Instantiate a location *)
            (match h with
            | CtxtConVar _ -> 
              type_error 
                (sprintf "Expect %s to be instantiated with a location con, but got %s" (pp_op op) (pp_con c))
            | CtxtLoc (eta, l) ->
              if loc <> l then 
                type_error (sprintf "Expect %s to be %s" (pp_loc loc) (pp_loc l))
              else
                let _ = wf_loc env l in
                Forall(tail_ctxt, (subst_ra ra (ConLoc l) (CVLoc eta)), 
                (subst_access_cap access_cap (ConLoc l) (CVLoc eta)),
                (subst_alloc_cap alloc_cap (ConLoc l) (CVLoc eta)))
            )
          | _ -> (* Instantiate a con *)
            (match h with (* h should be a be some β : k *)
            | CtxtConVar beta -> 
              let k = kind_of_con env c in
              if kind_of_con_var beta <> k then 
                type_error (sprintf "Expect %s to be of kind %s" (pp_con_var beta) (pp_kind k))
              else
                Forall(tail_ctxt, (subst_ra ra c beta), 
                (subst_access_cap access_cap c beta),
                (subst_alloc_cap alloc_cap c beta))
            | CtxtLoc (_, _) ->
              type_error 
                (sprintf "Expect %s to be instantiated with a non-location con, but got %s" (pp_op op) (pp_con c))
            )
          )
      )
    | _ -> type_error ("Expect " ^ (pp_op op) 
        ^ " to have some Forall type, but got " ^ (pp_ty op_type)))

(* ========================== Substitutions Begin ========================== *)
and get_con_ty (c : con) = 
  match c with
  | ConTy t -> t
  | _ -> type_error "Mismatched constructor and constructor variable"
and get_con_aty (c : con) =
  match c with
  | ConATy aty -> aty
  | _ -> type_error "Mismatched constructor and constructor variable"
and get_con_loc (c : con) =
  match c with
  | ConLoc loc -> loc
  | _ -> type_error "Mismatched constructor and constructor variable"
and get_con_cap (c : con) =
  match c with
  | ConCap cap -> cap
  | _ -> type_error "Mismatched constructor and constructor variable"
and get_con_acap (c : con) =
  match c with
  | ConACap acap -> acap
  | _ -> type_error "Mismatched constructor and constructor variable"

(* FIXME: These are not capture avoiding *)
and subst_ra (ra : reg_asgn) (c : con) (b : con_var) =
  List.map (fun (r, typ) -> (r, subst_ty typ c b)) ra

and subst_ty (typ : ty) (c : con) (b : con_var) =
  let rec subst t =
    match t with
    | TyCon tv -> 
      (match b with 
      | CVTy tv' -> if tv = tv' then (get_con_ty c) else t
      | _ -> t)
    | TTop -> TTop
    | Int -> Int
    | TyPtr loc -> TyPtr (subst_loc loc c b)
    | Exist (ta, access_cap, alloc_cap, t') ->
      Exist (ta, (subst_access_cap access_cap c b), 
      (subst_alloc_cap alloc_cap c b), subst t')
    | Forall (ta, ra, access_cap, alloc_cap) ->
      Forall (ta, (subst_ra ra c b), (subst_access_cap access_cap c b), 
      (subst_alloc_cap alloc_cap c b)) in
  subst typ

and subst_loc (loc : location) (c : con) (b : con_var) =
  match loc with
  | LocCon lv -> (match b with
    | CVLoc lv' -> if lv = lv' then (get_con_loc c) else loc
    | _ -> loc)
  | LocB -> loc
  | LocNext l -> LocNext (subst_loc l c b)

and subst_access_cap (cap : access_cap) (c : con) (b : con_var) =
  match cap with
  | CapCon cv -> (match b with
    | CVCap cv' -> if cv = cv' then (get_con_cap c) else cap
    | _ -> cap)
  | CapNil -> CapNil
  | CapTy (loc, ty) -> CapTy (loc, subst_ty ty c b)
  | CapATy (loc, aty) -> CapATy (loc, subst_aty aty c b)
  | CapOTimes (cap1, cap2) -> 
    CapOTimes (subst_access_cap cap1 c b, subst_access_cap cap2 c b)
  | CapWedge (cap1, cap2) -> 
    CapWedge (subst_access_cap cap1 c b, subst_access_cap cap2 c b)
  | CapFrac (cap1, cap2) -> 
    CapFrac (subst_access_cap cap1 c b, subst_access_cap cap2 c b)

and subst_alloc_cap (acap : alloc_cap) (c : con) (b : con_var) =
  match acap with
  | ACapCon acv -> (match b with
    | CVACap acv' -> if acv = acv' then (get_con_acap c) else acap
    | _ -> acap)
  | ACapNil -> ACapNil
  | ACap (location, pred) ->
    ACap (subst_loc location c b, pred)
  | ACapCons (acap1, acap2) ->
    ACapCons (subst_alloc_cap acap1 c b, subst_alloc_cap acap2 c b)

and subst_aty (atyp : aty) (c : con) (b : con_var) =
  match atyp with
  | ATyCon atyv -> (match b with
    | CVATy atyv' -> if atyv = atyv' then get_con_aty c else atyp
    | _ -> atyp)
  | ATyNil -> ATyNil
  | ATyCons (ty, rest) ->
    ATyCons (subst_ty ty c b, subst_aty rest c b)

(* Check if when ∆' = ω_1:κ_1,···,ω_n:κ_n, then we should have · ⊢ Ω_i :κ_i (for 1 <= i <= n) *)
and check_cons_delta cons delta' =
  (match cons, delta' with
    | cons_h :: cons_t, delta'_h :: delta'_t ->
      (match cons_h, delta'_h with 
      | ConLoc _, CtxtConVar (CVLoc _)
      | ConCap _, CtxtConVar (CVCap _)
      | ConTy _, CtxtConVar (CVTy _)
      | ConATy _, CtxtConVar (CVATy _)
      | ConACap _, CtxtConVar (CVACap _) -> 
        check_cons_delta cons_t delta'_t
      | _, _ -> type_error "cons and ∆' mismatch")
    | [], [] -> 
      ()
    | _, _ -> type_error "Length of cons and ∆' is different")

and subst_con (con : con) (c : con) (b : con_var) =
  match con with
  | ConLoc l -> ConLoc (subst_loc l c b)
  | ConCap cap -> ConCap (subst_access_cap cap c b)
  | ConTy ty -> ConTy (subst_ty ty c b)
  | ConATy aty -> ConATy (subst_aty aty c b)
  | ConACap acap -> ConACap (subst_alloc_cap acap c b)

(* FIXME: These are not simultaneous substitution *)
and subst_con_m (c : con) (cons : con list) (con_vars : con_var list) =
  let rec subst_m c cons con_vars =
    (match cons, con_vars with
    | [], [] -> c
    | consh :: const, con_vars_h :: con_vars_t ->
      subst_m (subst_con c consh con_vars_h) const con_vars_t
    | _, _ -> type_error "Length of cons and ∆ is different") in
  subst_m c cons con_vars

(* =========================== Substitutions End =========================== *)
and typeof_word env word =
  match word with
  | Label s -> lookup_label env s
  | Immediate _ -> Int
  | Ns -> TTop
  | Ptr _ -> 
    (* TODO: *)
    TTop
  | WordIns (word, con) -> typeof_operand env (OperandIns ((Word word), con))

(* Check if ra2 is a subset of ra1 *)
let rec subset_of ra2 ra1 =
  match ra2 with
  | [] -> true
  | h :: t -> 
    (match List.find_opt (fun x -> x = h) ra1 with
    | Some _ -> true && (subset_of t ra1)
    | None -> false)

let rec cap_rewrite_witness cap (witness : witness) =
  match witness with
  | [] -> cap
  | h :: [] ->
    let (rewrite, pos_ind) = h in
    (match pos_ind with
    | [] -> cap_rewrite cap rewrite
    | posh :: post ->
      if posh = 2 then
        (match cap with
        | CapOTimes (cap1, cap2) ->
          CapOTimes (cap1, cap_rewrite_witness cap2 [(rewrite, post)])
        | CapWedge (cap1, cap2) ->
          CapWedge (cap1, cap_rewrite_witness cap2 [(rewrite, post)])
        | CapFrac (cap1, cap2) ->
          CapFrac (cap1, cap_rewrite_witness cap2 [(rewrite, post)])
        | _ -> type_error "expecting a op cap")
      else
        (match cap with
        | CapOTimes (cap1, cap2) ->
          CapOTimes (cap_rewrite_witness cap1 [(rewrite, post)], cap2)
        | CapWedge (cap1, cap2) ->
          CapWedge (cap_rewrite_witness cap1 [(rewrite, post)], cap2)
        | CapFrac (cap1, cap2) ->
          CapFrac (cap_rewrite_witness cap1 [(rewrite, post)], cap2)
        | _ -> type_error "expecting a op cap")
    )
  | h :: tail_witness -> (* cap-rewrite-cons *)
    let cap' = cap_rewrite_witness cap [h] in
    cap_rewrite_witness cap' tail_witness

and cap_rewrite cap (rewrite : rewrite) =
  match rewrite with
  | Drop -> (match cap with
    | CapOTimes (cap1, _) | CapWedge (cap1, _) | CapFrac (cap1, _) -> cap1
    | _ -> type_error "expecting a op cap"
    )
  | Dup -> CapWedge (cap, cap)
  | Comm -> (match cap with
    | CapOTimes (cap1, cap2) -> CapOTimes (cap2, cap1)
    | CapWedge (cap1, cap2) -> CapWedge (cap2, cap1)
    | CapFrac (cap1, cap2) -> CapFrac (cap2, cap1)
    | _ -> type_error "expecting a op cap"
    )
  | Assoc -> (match cap with
    | CapOTimes (CapOTimes(cap1, cap2), cap3) -> 
        CapOTimes (cap1, CapOTimes(cap2, cap3))
    | CapOTimes (CapWedge(cap1, cap2), cap3) ->
        CapOTimes (cap1, CapWedge(cap2, cap3))
    | CapOTimes (CapFrac(cap1, cap2), cap3) ->
        CapOTimes (cap1, CapFrac(cap2, cap3))
    | CapWedge (CapOTimes (cap1, cap2), cap3) ->
        CapWedge (cap1, CapOTimes(cap2, cap3))
    | CapWedge (CapWedge (cap1, cap2), cap3) ->
        CapWedge (cap1, CapWedge(cap2, cap3))
    | CapWedge (CapFrac (cap1, cap2), cap3) ->
        CapWedge (cap1, CapFrac (cap2, cap3))
    | CapFrac (CapWedge (cap1, cap2), cap3) ->
        CapFrac (cap1, CapWedge (cap2, cap3))
    | CapFrac (CapOTimes (cap1, cap2), cap3) ->
        CapFrac (cap1, CapOTimes (cap2, cap3))
    | CapFrac (CapFrac (cap1, cap2), cap3) ->
        CapFrac (cap1, CapWedge (cap2, cap3))
    | _ -> type_error "bad pattern"
  )
  | Distr1 -> (match cap with
    | CapOTimes (CapWedge (cap1, cap2), cap3) ->
        CapWedge (CapOTimes (cap1, cap3), CapOTimes (cap2, cap3))
    | _ -> type_error "expecting a (C1∧C2)⊗C3"
  )
  | Distr2 -> (match cap with
    | CapWedge (CapOTimes (cap1, cap3), CapOTimes (cap2, cap3'))
      when cap3 = cap3' ->
        CapOTimes (CapWedge (cap1, cap2), cap3)
    | _ -> type_error "expecting a (C1⊗C3)∧(C2⊗C3)"
  )
  | Borrow -> (match cap with
    | CapWedge (c1, c2) -> CapOTimes (c1, CapFrac (c2, c1))
    | _ -> type_error "expecting a C1∧C2 ")
  | Return -> (match cap with
    | CapOTimes (c1, CapFrac(c2, c1'))
      when c1' = c1 ->
      CapWedge (c1, c2)
    | _ -> type_error "expecting a C1⊗(C2/C1)")
  | _ -> failwith "TODO" (* TODO: missing split and join *)


(* add tv to typing variables, raise type error if its a duplicate *)
(* let update_ty_asgn (env : static_env) tv =
  let (l, r, ta, cap) = env in
  if (List.mem (TAITVar tv) ta) then 
    type_error "duplicate type variable"
  else
    (l, r, ta, cap) *)

(******************************** Typing rules ********************************)
(* Typecheck if two stack types are equal *)
(* let typecheck_stack_eq _ (sty1 : stack_ty) (sty2 : stack_ty) =
  let sty1_serialized = serialize_sty sty1 in
  let sty2_serialized = serialize_sty sty2 in
  if not (sty1_serialized = sty2_serialized) then
    type_error ("Can't typeecheck the equality of " ^ (pp_sty sty1) ^ " and " ^ (pp_sty sty2)) *)
  
(* Typecheck if register assignment ra1 is subtype of ra2, i.e. ra2 is a subset 
   of ra1, i.e. ra1 <= ra2 *)
let typecheck_subtype _ ra1 ra2 =
  if subset_of ra2 ra1 then ()
  else type_error ("ra1 is not a subtype of ra2 (normal registers)")

(* let typecheck_subtype env ra1 ra2 =
  let (st1, normal_reg1) = ra1 in
  let (st2, normal_reg2) = ra2 in
  print_endline ("ra1: " ^ (pp_reg_asgn ra1));
  print_endline ("ra2: " ^ (pp_reg_asgn ra2));
  if not (subset_of normal_reg2 normal_reg1) then
    type_error ("ra1 is not a subtype of ra2 (normal registers)")
  else
    let _ = typecheck_each_ra env normal_reg1 in
    let _ = typecheck_each_ra env normal_reg2 in
    let _ = typecheck_stack_eq env st1 st2 in
    () *)

(* typecheck each type in a type list `l` *)
let typecheck_each_type env l =
  let _ = List.map (fun x -> wf_ty env x) l in
  ()

let rec location_minus_opt (l : location) i =
  if i = 0 then Some l
  else
    match l with
    | LocNext l' ->
      location_minus_opt l' (i - 1)
    | _ -> None


let rec location_minus (l : location) i =
  if i = 0 then l
  else
    match l with
    | LocNext l' ->
      location_minus l' (i - 1)
    | _ -> type_error (sprintf "location is not long enough")

let rec location_plus (l : location) i =
  if i = 0 then l
  else LocNext (location_plus l (i - 1))

let typeafter ty = 
  match ty with
  | Exist (_, _, _, _) -> TTop
  | _ -> ty

(* seq, jmp, halt *)
let rec wf_seq (env : static_env) ins_seq = 
  match ins_seq with
  | Jmp v -> (* jmp *)
    print_endline ("-------------------------typechecking-------------------------");
    print_endline ("current env " ^ pp_env env);
    print_endline ("type checking Jmp " ^ (pp_op v));
    let current_ra = get_ra env in
    let operand_type = typeof_operand env v in
    (match operand_type with (* Type of v must be a code type *)
    | Forall (ta, ra, c, _) -> (* TODO: Jump does not use alloc cap? *)
      let _ = typecheck_subtype env current_ra ra in
      if ta <> [] then
        type_error ("Cannot jmp because there's un-instantiated (stack) type variables: " ^ (pp_ty_asgn ta))
      else
      let current_cap = get_access_cap env in
      if current_cap <> c then
        type_error 
          (sprintf "Cannot jmp because the current access cap (%s) is different from expected (%s)" (pp_cap current_cap) (pp_cap c))
      else
        ()
    | _ -> type_error ("Expect " ^ (pp_op v) ^ " to have some Forall type, but got " ^ (pp_ty operand_type)))
  | Halt t -> (* halt *)
    let rdi_ty = typeof_reg env Rdi in
    if (not (rdi_ty = t)) then type_error (type_err_expect "rdi" t rdi_ty)
  | InstructionSeq (ins_line, ins_seq) -> (* seq *)
    let ins = (match ins_line with
      | InstructionLine (ins, _) -> ins
      | Comment _ -> Nop) in
    let (new_env, _) = typeof_instruction env ins in
    wf_seq new_env ins_seq

and typeof_instruction (env : static_env) ins =
  print_endline ("typechecking: " ^ pp_instruction ins);
  print_endline ("current env " ^ pp_env env);
  print_endline ("-------------------------typecheck-------------------------");
  match ins with
  (* mov *)
  | Mov (rd, op) ->
    let op_type = typeof_operand env op in
    (match op with
    | Reg rs -> 
      let new_env = update_register env rs (typeafter op_type) in
      let new_env = update_register new_env rd op_type in
      (new_env, ())
    | _ -> 
      let new_env = set_ra env (update_register_asgn (get_ra env) rd op_type) in
      (new_env, ()))
    
  (* aop *)
  | Aop (_, r, op) -> 
    let r_ty = typeof_reg env r in
    let op_ty = typeof_operand env op in
    let r_typecheck = (r_ty = Int) in
    let op_typecheck = (op_ty = Int) in
    if r_typecheck && op_typecheck then
      (env, ())
    else
      if not r_typecheck then
        type_error (type_err_expect (pp_reg r) Int r_ty)
      else 
        if not op_typecheck then
          type_error (type_err_expect (pp_op op) Int op_ty)
        else 
          (env, ())
  (* ld *)
  | Ld (rd, rs, i) -> 
    let rs_type = typeof_reg env rs in
    (match rs_type with
      | TyPtr l -> 
        let l'_opt = location_minus_opt l i in
        let l' = (match l'_opt with
        | Some l' -> l'
        | None -> type_error (sprintf "Location %s is not long enough for ld." (pp_loc l))) in
        let c = get_access_cap env in
        (match c with
          | CapOTimes ((CapTy (l'', tao)), c') when l'' = l' ->
            let tao' = typeafter tao in
            let new_env = update_register env rd tao in
            let new_env = set_access_cap new_env (CapOTimes (CapTy (l', tao'), c')) in
            (new_env, ())
          | _ -> type_error (sprintf "Expecting access cap to look like C=l':τ⊗C'"))
      | _ -> type_error (sprintf "Expect %s to have some ptr type but got %s." (pp_reg rs) (pp_ty rs_type))
    )
  (* malloc *)
  | Malloc (loc_var, i) ->
    let l = location_plus (LocCon loc_var) i in
    let rec malloc_cap ita idx =
      if idx = i + 1 then get_access_cap env else
      CapOTimes(CapTy(location_plus ita idx, TTop), malloc_cap ita (idx + 1)) in
    let c' = malloc_cap (LocCon loc_var) 1 in
    let cur_alloc_cap = get_alloc_cap env in
    let new_alloc_cap = ACapCons (ACap(l, PTuplestart), 
      ACapCons (ACap((LocCon loc_var), PTupleend), cur_alloc_cap)) in
    let new_env = update_register env Rax (TyPtr l) in
    let new_env = 
      set_alloc_cap (set_access_cap new_env c') new_alloc_cap in
    let new_env = insert_con_var new_env (CVLoc loc_var) in
    (new_env, ())
  (* bop *)
  | Bop (_, reg, op) ->
    let reg_type = typeof_reg env reg in
    if reg_type <> Int then type_error (type_err_expect (pp_reg reg) Int reg_type)
    else
    let current_ra = get_ra env in
    let operand_type = typeof_operand env op in
    (match operand_type with (* Type of v must be a code type *)
    | Forall (ta, ra, c, _) -> (* TODO: Jump does not use alloc cap? *)
      let _ = typecheck_subtype env current_ra ra in
      if ta <> [] then
        type_error ("Cannot bop because there's un-instantiated (stack) type variables: " ^ (pp_ty_asgn ta))
      else
        let current_cap = get_access_cap env in
        if current_cap <> c then
          type_error 
            (sprintf "Cannot bop because the current access cap (%s) is different from expected (%s)" (pp_cap current_cap) (pp_cap c))
        else
          (env, ())
    | _ -> type_error ("Expect " ^ (pp_op op) ^ " to have some Forall type, but got " ^ (pp_ty operand_type)))
  (* salloc *)
  | Salloc i ->
    let sp_type = typeof_reg env Rsp in
    (match sp_type with
    | TyPtr l ->
      let acap = get_alloc_cap env in
      (match acap with
      | ACapCons (ACap(l', PStacktop), acap') when l = l' -> 
        let l' = location_plus l i in
        let rec salloc_cap loc idx =
          if idx = i + 1 then get_access_cap env else
          CapOTimes(CapTy(location_plus loc idx, TTop), salloc_cap loc (idx + 1))
        in
        let c' = salloc_cap l 1 in
        let acap'' = ACapCons (ACap(l', PStacktop), acap') in
        let env' = update_register env Rsp (TyPtr l') in
        let env' = set_access_cap env' c' in
        let env' = set_alloc_cap env' acap'' in
        (env', ())
      | _ -> type_error (sprintf "Expecting alloc cap to look like l:stacktop,Ξ′"))
    | _ -> type_error ("Expecting RSP to have some PTR type"))
  (* sfree *)
  | Sfree i ->
    let sp_type = typeof_reg env Rsp in
    (match sp_type with
    | TyPtr l ->
      let acap = get_alloc_cap env in
      (match acap with
      | ACapCons (ACap(l', PStacktop), acap') when l = l' -> 
        let l' = location_minus l i in
        let rec strip_first_i cc idx =
          if idx = i + 1 then cc else
          (match cc with
          | CapOTimes (CapTy (ll, _), cc') when ll = (location_plus l' idx) ->
            strip_first_i cc' (idx + 1)
          | _ -> type_error ("Expecting access cap to look like l′:τ^1 ⊗···⊗l′:τ^i ⊗C′"))
        in
        let c' = strip_first_i (get_access_cap env) 1 in
        let acap'' = ACapCons (ACap(l', PStacktop), acap') in
        let env' = update_register env Rsp (TyPtr l') in
        let env' = set_access_cap env' c' in
        let env' = set_alloc_cap env' acap'' in
        (env', ())
      | _ -> type_error (sprintf "Expecting alloc cap to look like l:stacktop,Ξ′"))
    | _ -> type_error ("Expecting RSP to have some PTR type"))
  | Pack (cons, _, _, _, delta') ->
    (* pack [cons | c'[cons/delta'], ksi'[cons/delta']], r as ∃[delta' | c', ksi'] *)
    let _ = List.map
      (function | CtxtConVar cv -> cv
        | CtxtLoc (_, _) -> type_error "Encountered η=l in ∆'") delta' in
    let _ = check_cons_delta cons delta' in
    (* TODO: This is not finished *)
    (* let r_type = typeof_reg env r in *)

    (env, ())
  (* unpack *)
  | Unpack (delta', r) ->
    let r_type = typeof_reg env r in
    (match r_type with
    | Exist (delta'', c', ksi', tao) when delta'' = delta' ->
      let delta = get_ta env in
      let c = get_access_cap env in
      let ksi = get_alloc_cap env in
      let new_env = set_ta env (concat_ctxt delta' delta) in
      let new_env = update_register  new_env r tao in
      let new_env = set_access_cap new_env (CapOTimes (c', c)) in
      let new_env = set_alloc_cap new_env (ACapCons (ksi, ksi')) in
      (new_env, ())
    | _ -> type_error 
      (sprintf "Expecting type of %s to be some Exist, but got %s" (pp_reg r) (pp_ty r_type)))
  (* Nop *)
  | Nop -> (env, ())
  (* st *)
  | St (rd, rs, i) ->
    let rd_type = typeof_reg env rd in
    let tao = typeof_reg env rs in
    (match rd_type with
    | TyPtr l ->
      let l' = location_minus l i in
      (match (get_access_cap env) with
      | CapOTimes (CapTy (l'', _), c') when l'' = l' ->
        let tao' = typeafter tao in
        let env' = update_register env rd tao' in
        let new_cap = CapOTimes (CapTy (l', tao), c') in
        (set_access_cap env' new_cap, ())
      | _ -> type_error (sprintf "Expecting C to look like l':τ′′ ⊗C'"))
    | _ -> type_error (sprintf "Expecting %s to have some PTR type but got %s"
      (pp_reg rd) (pp_ty rd_type)))
  | MakeStack (loc_var, _) ->
    if in_context_bool (get_ta env) (CVLoc loc_var) then
      type_error (sprintf "Location variable %s is already used" (pp_loc_var loc_var))
    else
      let env' = update_register env Rsp (TyPtr (LocCon loc_var)) in
      let acap = get_alloc_cap env in
      let acap' = ACapCons 
        (ACap(LocCon loc_var, PStacktop), ACapCons (ACap(LocCon loc_var, PStackbase), acap)) in
      let env'' = set_alloc_cap env' acap' in
      (env'', ())
  | FreeStack -> 
    let sp_type = typeof_reg env Rsp in
    (match sp_type with
    | TyPtr l ->
      (match get_alloc_cap env with
      | ACapCons (ACap(ll, PStacktop), ACapCons(ACap(ll', PStackbase), ksi'))
        when ll = l && ll' = ll ->
        (set_alloc_cap env ksi', ())
      | _ -> type_error "Expecting alloc cap to look like l:stacktop,l:stackbase,Ξ′")
    | _ -> type_error ("Expecting RSP to have some PTR type"))
  | Coerce witness ->
    let c = get_access_cap env in
    let c' = cap_rewrite_witness c witness in
    ((set_access_cap env c'), ())

(* reg *)
and typeof_reg env reg =
  lookup_register env reg

(* typecheck code *)
and typeof_code (env : static_env) code =
  match code with
  | Code (ta, ra, cap, acap, ins_seq) -> 
    let _ = wf_reg_asgn env ra in
    let _ = wf_cap env cap in
    let _ = wf_acap env acap in
    let new_env = ((get_la env), ra, ta, cap, acap) in
    wf_seq new_env ins_seq

(* Simply get the type annotation of `code`, without checking it *)
and type_annotation_of_code code =
  match code with
  | Code (ta, ra, cap, acap, _) -> 
    Forall (ta, ra, cap, acap)

and wf_ctxt (env : static_env) (ctxt : ty_asgn) = 
  match ctxt with
  | [] -> ()
  | (CtxtConVar _) :: ctxt' ->
    wf_ctxt env ctxt'
  | (CtxtLoc (_, l)) :: ctxt' ->
    let _ = wf_ctxt env ctxt' in
    wf_loc (set_ta env ctxt) l
    (* TODO: Isn't delta' a subset of delta? *)

and wf_reg_asgn (env : static_env) (ra : reg_asgn) =
  match ra with
  | [] -> ()
  | h :: t ->  
    let (_, typ) = h in
    let _ = wf_ty env typ in
    wf_reg_asgn env t

(* and wf_cap (env : static_env) (c : cap) = *)


(* Second pass: typecheck the program *)
and typecheck_prog env (ast : code_block_seq) =
  match ast with
  | CodeBlockSeq cb -> 
    (match cb with
    | CodeBlock (_, code) ->
      let _ = typeof_code env code in
      ())
  | CodeBlockSeqCons (cb, cbs) -> 
    (match cb with
    | CodeBlock (_, code) ->
      let _ = typeof_code env code in
      let next_env = set_la empty_env (get_la env) in
      (* the environment for typechecking next label is empty except the label assignments *)
      typecheck_prog next_env cbs)

(* First pass: store all the type annotation info of labels into environment *)
and first_pass env (ast : code_block_seq) : static_env =
  match ast with
  | CodeBlockSeq cb -> 
    (match cb with
    | CodeBlock (name, code) ->
      let code_annotation = type_annotation_of_code code in
      let new_env = set_la env 
        (extend_label (get_la env) name code_annotation) in
      new_env)
  | CodeBlockSeqCons (cb, cbs) -> 
    (match cb with
    | CodeBlock (name, code) ->
      let code_annotation = type_annotation_of_code code in
      let new_env = set_la env 
        (extend_label (get_la env) name code_annotation) in
      first_pass new_env cbs)
    
let typecheck intputFile =
  let ast = parseFile intputFile in
  (* Enter from the `instruction_seq` of "_main" *)
  (* let main = lookup_code_block ast "_main" in *)
  let env = first_pass empty_env ast in
  print_endline "===============================================================";
  print_endline (pp_env env);
  let _ = typecheck_prog env ast in
  print_endline "Typechecked" ;
  ast