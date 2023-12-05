open SstalParser.Main
open Ast
open Prettyprint
open Utils
exception TypeError of string
exception StackTypeError of string

let type_error s = 
  raise (TypeError s)

type static_env = label_asgn * reg_asgn * ty_asgn

let empty_env = ([], (Nil, []), [])

(****************************** Helper functions ******************************)
(* Return a string about a type mismatch error *)
let type_err_expect str expected_ty actual_ty =
  Printf.sprintf "Expect %s to have type %s, but got %s\n" str (pp_ty expected_ty) (pp_ty actual_ty)

let get_str_of_label = function
  | LAdr _ -> failwith "Invalid label"
  | LStr s -> s

(* Returns the `code` corresponding to label `l`, in the code_block_seq `ast` *)
let rec lookup_code_block ast l =
  match ast with
  | CodeBlockSeq code_block -> 
    (match code_block with
      | CodeBlock (label, code) -> 
        if get_str_of_label label = l then code
        else failwith ("Cannot find label" ^ l))
  | CodeBlockSeqCons (h, t) ->
    (match h with
      | CodeBlock (label, code) -> 
        if get_str_of_label label = l then code
        else lookup_code_block t l)

let get_label_assignment env =
  let (label_assignment, _, _) = env in
  label_assignment

let lookup_label (env: static_env) (x : string) : ty =
  let (label_assignment, _, _) = env in
  match List.assoc_opt x label_assignment with
  | Some t -> t
  | None -> failwith ("Unbound label: " ^ x)

(* TODO: Avoid duplication? *)
let extend_label label_assignment label_name typ =
  (label_name, typ) :: label_assignment

let lookup_register (env : static_env) (r : reg) : ty =
  let (_, (_, rf), _) = env in
  match (List.assoc_opt r rf) with
  | Some t -> t
  | None -> failwith ("Unbound register: " ^ (pp_reg r))

(* Return a new register assignment with `reg` having type `t` *)
let update_register_asgn (register_asgn : reg_asgn) (r : reg) (t : ty) =
  let (sp_type, normal_reg) = register_asgn in
  (sp_type, (update_assoc_list r t normal_reg))

(* All the elements in l1 that is not in l2 *)
let diff l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1

(* free vars of a register asgn *)
let rec free_vars_ra (ra : reg_asgn) =
  let (sty, normal_reg) = ra in
  (free_vars_sty sty) @ (List.concat_map (fun x -> let (_, t) = x in free_vars_ty t) normal_reg)

and free_vars_sty sty = 
  match sty with
  | StackTypeVar v -> [TAISTVar v]
  | Nil -> []
  | Cons (typ, sty) -> (free_vars_ty typ) @ (free_vars_sty sty)
  | Append (sty1, sty2) -> (free_vars_sty sty1) @ (free_vars_sty sty2)

(* return the free variables in a `ty` as a list *)
and free_vars_ty (typ : ty) =
  match typ with
  | Var v -> [TAITVar v] (* Type variable *)
  | Int -> []
  | TypeList l -> List.concat_map free_vars_ty l
  | Forall (ta, ra) -> 
    let l1 = free_vars_ra ra in
    let l2 = ta in
    diff l1 l2
  | Exist (tv, typp) -> diff (free_vars_ty typp) [TAITVar tv] 
  | TPtr sty -> free_vars_sty sty
  | TTop -> []

(* To make life easy *)
let (@@) sty1 sty2 = Append (sty1, sty2)
let (++) ty sty = Cons (ty, sty)


(* Serialize a stack_ty into a list of stack_item (either stack type variable or ty) *)
let rec serialize_sty sty = 
  match sty with
  | Cons (ty, sty') -> (SITy ty) :: (serialize_sty sty')
  | Append (sty1, sty2) -> (serialize_sty sty1) @ (serialize_sty sty2)
  | Nil -> []
  | StackTypeVar _ -> [SISty sty]

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


let rec deserialize_sty l =
  match l with
  | h :: t ->
    (match h with
    | SITy tao -> Cons (tao, deserialize_sty t)
    | SISty rho -> 
      (match t with
      | _ :: _ -> Append (rho, deserialize_sty t)
      | [] -> rho))
  | [] -> Nil

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
let update_register env reg typ =
  let (l, r, t) = env in
  let r' = update_register_asgn r reg typ in
  (l, r', t)

(* delete the register assignment of reg from env *)
let delete_register (env : static_env) reg =
  let (l, (sp, rf), t) = env in
  let rf' = List.filter (fun x -> let (reg', _) = x in reg' <> reg) rf in
  (l, (sp, rf'), t)

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

let typecheck_ty env typ =
  let free_vars = free_vars_ty typ in
  let (_, _, env_vars) = env in
  let unbound_free_vars = diff free_vars env_vars in
  if unbound_free_vars <> [] then
    type_error ((pp_ty_asgn unbound_free_vars) ^ " is unbound") (* TODO *)

(* stype *)
let rec typecheck_sty env sty =
  let free_vars = free_vars_sty sty in
  let (_, _, env_vars) = env in
  let unbound_free_vars = diff free_vars env_vars in
  if unbound_free_vars <> [] then
    type_error ((pp_ty_asgn unbound_free_vars) ^ " is unbound in stack type " ^ (pp_sty sty)) (* TODO *)

(* Iterate through normal registers and typecheck each type *)
and typecheck_each_ra env l =  
  match l with
  | [] -> ()
  | h :: t -> 
    let (_, typ) = h in
    let _ = typecheck_ty env typ in
    typecheck_each_ra env t

(* rftype *)
let typeof_reg_asgn env reg_assignments =
  let (stack_type, ra) = reg_assignments in
  let _ = typecheck_sty env stack_type in  
  let _ = typecheck_each_ra env ra in
  ()

(* Check if ra2 is a subset of ra1 *)
let rec subset_of ra2 ra1 =
  match ra2 with
  | [] -> true
  | h :: t -> 
    (match List.find_opt (fun x -> x = h) ra1 with
    | Some _ -> true && (subset_of t ra1)
    | None -> false)

(* add tv to typing variables, raise type error if its a duplicate *)
let update_ty_asgn (env : static_env) tv =
  let (l, r, ta) = env in
  if (List.mem (TAITVar tv) ta) then 
    type_error "duplicate type variable"
  else
    (l, r, ta)

(******************************** Typing rules ********************************)
(* Typecheck if two stack types are equal *)
let typecheck_stack_eq _ (sty1 : stack_ty) (sty2 : stack_ty) =
  let sty1_serialized = serialize_sty sty1 in
  let sty2_serialized = serialize_sty sty2 in
  if not (sty1_serialized = sty2_serialized) then
    type_error ("Can't typeecheck the equality of " ^ (pp_sty sty1) ^ " and " ^ (pp_sty sty2))
  
(* Typecheck if register assignment ra1 is subtype of ra2, i.e. ra2 is a subset 
   of ra1, i.e. ra1 <= ra2 *)
let typecheck_subtype env ra1 ra2 =
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
    ()

(* typecheck each type in a type list `l` *)
let typecheck_each_type env l =
  let _ = List.map (fun x -> typecheck_ty env x) l in
  ()

(* seq, jmp, halt *)
let rec typeof_ins_seq env ins_seq = 
  match ins_seq with
  | Jmp v -> (* jmp *)
    print_endline ("-------------------------typecheck-------------------------");
    print_endline ("current env " ^ pp_env env);
    print_endline ("type checking Jmp " ^ (pp_op v));
    let (_, r, _) = env in
    let operand_type = typeof_operand env v in
    (match operand_type with (* Type of v must be a code type *)
    | Forall (ta, ra) ->
      let _ = typecheck_subtype env r ra in
      if ta <> [] then
        type_error ("Cannot jmp because there's un-instantiated (stack) type variables: " ^ (pp_ty_asgn ta))
      else
      ()
    | _ -> type_error ("Expect " ^ (pp_op v) ^ " to have some Forall type, but got " ^ (pp_ty operand_type)))
  | Halt t -> (* halt *)
    let rax_ty = typeof_reg env Rdi in
    if (not (rax_ty = t)) then type_error (type_err_expect "rdi" t rax_ty)
  | InstructionSeq (ins_line, ins_seq) -> (* seq *)
    let ins = (match ins_line with
      | InstructionLine (ins, _) -> ins
      | Comment _ -> Nop) in
    let (new_env, _) = typeof_instruction env ins in
    typeof_ins_seq new_env ins_seq

and typeof_instruction (env : static_env) ins =
  print_endline ("typechecking: " ^ pp_instruction ins);
  print_endline ("current env " ^ pp_env env);
  print_endline ("-------------------------typecheck-------------------------");
  match ins with
  (* mov *)
  | Mov (reg, op) -> 
    let (l, r, t) = env in
    let op_type = typeof_operand env op in
    let new_env = (l, update_register_asgn r reg op_type, t) in
    (new_env, ())
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
    | TypeList l -> 
      let n = List.length l in
      if (1 <= i && i <= n) then
        let taoi = List.nth l i in
        let (l, ra, t) = env in
        let new_ra = update_register_asgn ra rd taoi in
        ((l, new_ra, t), ())
      else
        type_error ("offset in " ^ (pp_instruction ins) ^ " is invalid because the tuple only has length " ^ (string_of_int n))
    | _ -> type_error ("expect " ^ (pp_reg rs) ^ " to have type Tuple but got " ^ (pp_ty rs_type)))
  (* malloc *)
  (* assumes the list of types can all fit in a word *)
  | Malloc tuple ->
    let _ = typecheck_each_type env tuple in
    let (l, ra, t) = env in
    let new_ra = update_register_asgn ra Rax (TypeList tuple) in
    ((l, new_ra, t), ())
  (* bop *)
  | Bop (_, reg, op) ->
    let reg_type = typeof_reg env reg in
    if reg_type <> Int then type_error (type_err_expect (pp_reg reg) Int reg_type)
    else
    let (_, ra1, _) = env in
    let operand_type = typeof_operand env op in
    (match operand_type with (* Type of op must be a code type *)
    | Forall (ta, ra2) ->
      if ta <> [] then
        type_error ("Cannot jmp because there's un-instantiated (stack) type variables: " ^ (pp_ty_asgn ta))
      else
      let _ = typecheck_subtype env ra1 ra2 in
      (env, ())
    | _ -> type_error ("Expect " ^ (pp_op op) ^ " to have some Forall type, but got " ^ (pp_ty operand_type)))
  (* salloc *)
  | Salloc n ->
    let (l, ra, t) = env in
    let (sigma, normal_reg) = ra in
    let rec prepend_n_tops n new_simga = (* prepend n top types to sigma *)
      if n = 0 then new_simga
      else
        Cons (TTop, prepend_n_tops (n - 1) new_simga)
    in
    let sigma' = prepend_n_tops n sigma in
    let new_ra = (sigma', normal_reg) in
    ((l, new_ra, t),())
  (* sfree *)
  | Sfree n ->
    let sigma1 = get_stack_type env in
    let sigma2 = deserialize_sty (strip_n_types (serialize_sty sigma1) n) in
    let new_env = update_sp env sigma2 in
    (new_env, ())
  (* sst1 *)
  | Sstsp (_, rs, i) ->
    if i < 0 then failwith "Encountered negative offset for sst" else
    let rs_type = typeof_reg env rs in
    let sigma1 = get_stack_type env in
    let sigma1' = deserialize_sty (insert_ty_to_sit rs_type (serialize_sty sigma1) ((Int.abs i))) in
    (update_sp env sigma1', ())
  (* sst2 *)
  | Sst (rd, rs, i) ->
    let rd_type = typeof_reg env rd in
    (match rd_type with
    | TPtr sigma3 ->
      let rs_type = typeof_reg env rs in
      let sigma1 = get_stack_type env in
      let sigma2 = deserialize_sty (head_of_sit (serialize_sty sigma1) (serialize_sty sigma3) []) in
      let sigma5 = deserialize_sty (insert_ty_to_sit rs_type (serialize_sty sigma3) ((Int.abs i))) in
      let env' = update_sp env (Append (sigma2, sigma5)) in
      let env'' = update_register env' rd (TPtr sigma5) in
      (env'', ())
    | _ -> type_error ("expect " ^ (pp_reg rd) ^ "to have some ptr type but got " ^ (pp_ty rd_type)))
  (* get-sp *)
  | Movsp2 (rd, _) ->
    let sigma = get_stack_type env in
    (update_register env rd (TPtr sigma), ())
  (* set-sp *)
  | Movsp1 (_, rs) ->
    let rs_type = typeof_reg env rs in
    let sigma1 = get_stack_type env in
    (match rs_type with
    | TPtr sigma2 -> 
      let _ = head_of_sit (serialize_sty sigma1) (serialize_sty sigma2) [] in (* this is sigma3 *)
      (update_sp env sigma2, ())
    | _ -> type_error ("expect " ^ (pp_reg rs) ^ "to have some ptr type but got " ^ (pp_ty rs_type)))
  (* sld1 *)
  | Sldsp (rd, _, i) ->
    let sigma1 = get_stack_type env in
    let taoi = get_ith_type (serialize_sty sigma1) i in
    (update_register env rd taoi, ())
  (* sld2 *)
  | Sld (rd, rs, i) ->
    let rs_type = typeof_reg env rs in
    let sigma1 = get_stack_type env in
    (match rs_type with
    | TPtr sigma3 ->
      let _ = deserialize_sty (head_of_sit (serialize_sty sigma1) (serialize_sty sigma3) []) in
      let taoi = get_ith_type (serialize_sty sigma3) i in
      (update_register env rd taoi, ())
    | _ -> type_error ("expect " ^ (pp_reg rs) ^ "to have some ptr type but got " ^ (pp_ty rs_type)))
  (* unpack *)
  | Unpack (tv, rd, v) ->
    let v_type = typeof_operand env v in
    (match v_type with
    | Exist (alpha, tao) ->
      if alpha <> tv then type_error ("expect type variable of unpack to be " ^ (match alpha with TVar s -> s)) else
      let env' = update_register env rd tao in
      let env'' = update_ty_asgn env' alpha in
      (env'', ())
    | _ -> type_error (Printf.sprintf "expect %s to have some Exist type but got %s" (pp_op v) (pp_ty v_type)))
  (* Nop *)
  | Nop -> (env, ())
  (* st *)
  | St (rd, rs, i) ->
    let rd_type = typeof_reg env rd in
    let rs_type = typeof_reg env rs in
    (match rd_type with
    | TypeList l ->
      let n = List.length l in
      if i >= 0 && i < n then
        if List.nth l i = rs_type then
          (env, ())
        else
          type_error (type_err_expect (pp_reg rs) (List.nth l i) rs_type)
      else
        type_error (Printf.sprintf "Invalid i, because %s has type %s" (pp_reg rd) (pp_ty rd_type))
    | _ -> type_error (Printf.sprintf "expect %s to have some Tuple type but got %s" (pp_reg rd) (pp_ty rd_type)))
  | MakeStack _ ->
    let env' = update_sp env Nil in
    let env'' = delete_register env' Rax in
    (env'', ())

(* reg *)
and typeof_reg env reg =
  lookup_register env reg

(* Within `ra`, rewrite occurences of `tv` into `typ` *)
and reg_asgn_substitute (ra : reg_asgn) (typ : ty) (tv : type_var) =
  let (sty, normal_reg) = ra in
  let normal_reg_substed = List.map (fun x -> (
    let (r, t) = x in
    (r, ty_substitute t typ tv)
  )) normal_reg in
  let sty_substed = sty_substitute sty typ tv in
  (sty_substed, normal_reg_substed)

(* Within `ra`, rewrite occurences of `stv` into `styp` *)
and reg_asgn_substitute_sty (ra : reg_asgn) (styp : stack_ty) (stv : stack_type_var) =
  let (sty, normal_reg) = ra in
  let normal_reg_substed = List.map (fun x -> (
    let (r, t) = x in
    (r, ty_substitute_sty t styp stv)
  )) normal_reg in
  let sty_substed = sty_substitute_sty sty styp stv in
  (sty_substed, normal_reg_substed)

(* Within `t`, rewrite occurences of `tv` into `typ` *)
and ty_substitute (t : ty) (typ : ty) (tv : type_var) =
  let rec subst (tao : ty) =
    if tao = typ then Var tv else
    match tao with
    | TypeList l -> TypeList (List.map (fun x -> subst x) l)
    | Forall (ta, ra) -> Forall (ta, (reg_asgn_substitute ra typ tv)) (* TODO: Avoid capture *)
    | Exist (alpha, tao) -> Exist (alpha, subst tao) (* TODO: Avoid capture *)
    | TPtr sty -> TPtr (sty_substitute sty typ tv)
    | Int | TTop -> tao
    | Var v -> if v = tv then typ else tao
  in
  subst t

(* Within `t`, rewrite occurences of `stv` into `styp` *)
and ty_substitute_sty (t : ty) (styp : stack_ty) (stv : stack_type_var) =
  let rec subst (tao : ty) =
    match tao with
    | TypeList l -> TypeList (List.map (fun x -> subst x) l)
    | Forall (ta, ra) -> Forall (ta, (reg_asgn_substitute_sty ra styp stv))
    | Exist (alpha, tao) -> (Exist (alpha, subst tao))
    | TPtr sty -> TPtr (sty_substitute_sty sty styp stv)
    | Int | TTop | Var _ -> tao
  in
  subst t

(* Within sty, rewrite occurences of `tv` into `typ` *)
and sty_substitute (sty : stack_ty) (typ : ty) (tv : type_var) =
  let rec subst (sigma : stack_ty) = 
    match sigma with
    | Nil -> Nil
    | Cons (tao, sigma') -> Cons (ty_substitute tao typ tv, subst sigma')
    | StackTypeVar _ -> sigma
    | Append (sigma1, sigma2) -> Append (subst sigma1, subst sigma2)
  in
  subst sty

(* Within sty, rewrite occurences of `stv` into `styp` *)
and sty_substitute_sty (sty : stack_ty) (styp : stack_ty) (stv : stack_type_var) =
  let rec subst (sigma : stack_ty) =
    (* if sigma = styp then StackTypeVar stv else *)
    match sigma with
    | Cons (tao, sigma') -> Cons ((ty_substitute_sty tao styp stv), subst sigma')
    | Append(sigma1, sigma2) -> Append (subst sigma1, subst sigma2)
    | StackTypeVar stv' -> if stv' = stv then styp else sigma
    | Nil -> Nil
  in
  subst sty

and typeof_word env w =
  match w with
  | Label l -> (* label *)
      (match l with
      | LAdr _ -> failwith "Encountered constant address"
      | LStr s -> lookup_label env s)
  | Immediate _ -> Int (* int *)
  | Ns -> TTop (* ns *)
  | Ptr _ -> (* ptr *)
    failwith "TODO"
  | WordPack (tao, w, exist_ty) -> (* pack *)
    let _ = typecheck_ty env tao in
    let w_type = typeof_word env w in
    (match exist_ty with
    | Exist (alpha, tao') ->
      let v_type_expect = ty_substitute tao' tao alpha in
      if v_type_expect = w_type then
        exist_ty
      else
        type_error (type_err_expect (pp_word w) v_type_expect w_type)
    | _ -> type_error (Printf.sprintf "expect %s to have some exist type" (pp_ty exist_ty)))

  (* tapp *)
  | WordTyPoly (w, typ) -> 
    let _ = typecheck_ty env typ in
    let w_type = typeof_word env w in
    (match w_type with
    | Forall (ta, ra) ->
      (match ta with
      | [] -> type_error ("The type assignments of " ^ (pp_word w) ^ " is empty.")
      | h :: t -> 
        (match h with
        | TAISTVar _ -> type_error ("The first variable of the type assignment of " ^ (pp_word w)
           ^ " is a stack type variable, but was expecting a type variable") 
        | TAITVar tv -> Forall (t, reg_asgn_substitute ra typ tv)))
    | _ -> type_error ("Expect " ^ (pp_word w) ^ " to have some Forall type, but got " ^ (pp_ty w_type)))
  | WordSTyPoly (w, sty) ->
    let _ = typecheck_sty env sty in
    let w_type = typeof_word env w in
    (match w_type with
    | Forall (ta, ra) ->
      (match ta with
      | [] -> type_error ("The type assignments of " ^ (pp_word w) ^ " is empty.")
      | h :: t -> 
        (match h with
        | TAITVar _ -> type_error ("The first variable of the type assignment of " ^ (pp_word w)
           ^ " is a type variable, but was expecting a stack type variable") 
        | TAISTVar tv -> Forall (t, reg_asgn_substitute_sty ra sty tv)))
    | _ -> type_error ("Expect " ^ (pp_word w) ^ " to have some Forall type, but got " ^ (pp_ty w_type)))

and typeof_operand env op =
  match op with
  | Reg reg -> typeof_reg env reg (* reg *)
  | Word word_val -> typeof_word env word_val 
  | OperandTyPoly (op, typ) -> 
    let _ = typecheck_ty env typ in
    let op_type = typeof_operand env op in
    (match op_type with
    | Forall (ta, ra) ->
      (match ta with
      | [] -> type_error ("The type assignments of " ^ (pp_op op) ^ " is empty.")
      | h :: t -> 
        (match h with
        | TAISTVar _ -> type_error ("The first variable of the type assignment of " ^ (pp_op op)
           ^ " is a stack type variable, but was expecting a type variable") 
        | TAITVar tv -> Forall (t, reg_asgn_substitute ra typ tv)))
    | _ -> type_error ("Expect " ^ (pp_op op) ^ " to have some Forall type, but got " ^ (pp_ty op_type)))
  | OperandSTyPoly (op, sty) ->
    let _ = typecheck_sty env sty in
    let w_type = typeof_operand env op in
    (match w_type with
    | Forall (ta, ra) ->
      (match ta with
      | [] -> type_error ("The type assignments of " ^ (pp_op op) ^ " is empty.")
      | h :: t -> 
        (match h with
        | TAITVar _ -> type_error ("The first variable of the type assignment of " ^ (pp_op op)
            ^ " is a type variable, but was expecting a stack type variable") 
        | TAISTVar tv -> Forall (t, reg_asgn_substitute_sty ra sty tv)))
    | _ -> type_error ("Expect " ^ (pp_op op) ^ " to have some Forall type, but got " ^ (pp_ty w_type)))
  | OperandPack (tao, v, exist_ty) ->
    let _ = typecheck_ty env tao in
    let v_type = typeof_operand env v in
    (match exist_ty with
    | Exist (alpha, tao') ->
      let v_type_expect = ty_substitute tao' tao alpha in
      if v_type_expect = v_type then
        exist_ty
      else
        type_error (type_err_expect (pp_op v) v_type_expect v_type)
    | _ -> type_error (Printf.sprintf "expect %s to have some exist type" (pp_ty exist_ty)))
  

(* typecheck code *)
and typeof_code (env : static_env) code =
  match code with
  | Code (type_assignments, reg_assignments, ins_seq) -> 
    let (l, r, _) = env in
    let _ = typeof_reg_asgn (l, r, type_assignments) reg_assignments in
    let _ = typeof_ins_seq (l, reg_assignments, type_assignments) ins_seq in
    Forall (type_assignments, reg_assignments)

(* Simply get the type annotation of `code`, without checking it *)
and type_annotation_of_code code =
  match code with
  | Code (type_assignments, reg_assignments, _) -> 
    Forall (type_assignments, reg_assignments)

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
      let next_env = (get_1 env, (Nil, []), []) in (* the environment for typechecking next label is empty except the label assignments *)
      let _ = typecheck_prog next_env cbs in
      ())

(* First pass: store all the type annotation info of labels into environment *)
and first_pass env (ast : code_block_seq) : static_env =
  let (l, r, t) = env in
  match ast with
  | CodeBlockSeq cb -> 
    (match cb with
    | CodeBlock (label, code) ->
      let label_name = (match label with
      | LAdr _ -> failwith "encountered constant address while storing type annotations"
      | LStr s -> s) in
      let code_annotation = type_annotation_of_code code in
      let new_env = ((extend_label l label_name code_annotation), r, t) in
      new_env
    )
  | CodeBlockSeqCons (cb, cbs) -> 
    (match cb with
    | CodeBlock (label, code) ->
      let label_name = (match label with
      | LAdr _ -> failwith "encountered constant address while storing type annotations"
      | LStr s -> s) in
      let code_annotation = type_annotation_of_code code in
      let new_env = ((extend_label l label_name code_annotation), r, t) in
      first_pass new_env cbs
    )
    
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