open TalParser.Main
open TalParser.Tal
open Prettyprint
open Utils
exception TypeError of string

let type_error s = 
  raise (TypeError s)

(* Return a string about a type mismatch error *)
let type_err_expect str expected_ty actual_ty =
  Printf.sprintf "Expect %s to have type %s, but got %s\n" str (pp_ty expected_ty) (pp_ty actual_ty)

type static_env = label_asgn * reg_asgn * ty_asgn

let empty_env = ([], (Nil, []), [])

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

(* type *)
let typecheck_ty env typ =
  let free_vars = free_vars_ty typ in
  let (_, _, env_vars) = env in
  let unbound_free_vars = diff free_vars env_vars in
  if unbound_free_vars <> [] then
    type_error ("" ^ "is unbound in type " ^ " ") (* TODO *)

(* stype *)
let rec typecheck_sty env sty =
  let free_vars = free_vars_sty sty in
  let (_, _, env_vars) = env in
  let unbound_free_vars = diff free_vars env_vars in
  if unbound_free_vars <> [] then
    type_error ("" ^ "is unbound in type " ^ " ") (* TODO *)

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

(* Flatten a sty from having Append to just having Cons *)
let rec flatten_sty sty =
  match sty with
  | Nil -> Nil
  | Cons (typ, sty) -> Cons (typ, flatten_sty sty)
  | Append (_, _) -> (
    (* let sty1_flattened = flatten_sty sty1 in
    let sty2_flattened = flatten_sty sty2 in
    concat_sty *)
    Nil (* TODO *)
  )
  | v -> v

(* Typecheck if two stack types are equal *)
let typecheck_stack_eq _ (sty1 : stack_ty) (sty2 : stack_ty) =
  let sty1_flattened = flatten_sty sty1 in
  let sty2_flattened = flatten_sty sty2 in
  if not (sty1_flattened = sty2_flattened) then
    type_error ("Can't typeecheck the equality of " ^ (pp_sty sty1) ^ " and " ^ (pp_sty sty2))
  
(* Typecheck if register assignment ra1 is subtype of ra2, i.e. ra2 is a subset 
   of ra1 *)
let typecheck_subtype env ra1 ra2 =
  let (st1, normal_reg1) = ra1 in
  let (st2, normal_reg2) = ra2 in
  if subset_of normal_reg2 normal_reg1 then type_error ((pp_reg_asgn ra1) 
    ^ " is not a subtype of " ^ (pp_reg_asgn ra2))
  else
    let _ = typecheck_each_ra env normal_reg1 in
    let _ = typecheck_each_ra env normal_reg2 in
    let _ = typecheck_stack_eq env st1 st2 in
    ()

(* seq, jmp, halt *)
let rec typeof_ins_seq env ins_seq = 
  match ins_seq with
  | Jmp _ -> (* jmp *)
      ()
  | Halt t -> (* halt *)
    let rax_ty = typeof_reg env Rax in
    if (not (rax_ty = t)) then type_error (type_err_expect "rax" t rax_ty)
  | InstructionSeq (ins_line, ins_seq) -> (* seq *)
    let ins = (match ins_line with
      | InstructionLine (ins, _) -> ins
      | Comment _ -> Nop) in
    let (new_env, _) = typeof_instruction env ins in
    typeof_ins_seq new_env ins_seq

and typeof_instruction (env : static_env) ins =
  print_endline ("typechecking: " ^ pp_instruction ins);
  print_endline ("current env " ^ pp_env env);
  match ins with
  | Mov (reg, op) -> (* mov *)
    let (l, r, t) = env in
    let op_type = typeof_operand env op in
    let new_env = (l, update_register_asgn r reg op_type, t) in
    (new_env, ())
  | Aop (_, r, op) -> (* aop *)
    let r_ty = typeof_reg env r in
    let op_ty = typeof_operand env op in
    let r_typecheck = r_ty = Int in
    let op_typecheck = op_ty = Int in
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
  | _ -> failwith "TODO"

(* reg *)
and typeof_reg env reg =
  lookup_register env reg

and typeof_word env w =
  match w with
  | Label l -> 
      (match l with
      | LAdr _ -> failwith "Encountered constant address"
      | LStr s -> lookup_label env s)
  | Immediate _ -> Int
  | _ -> failwith "TODO"

and typeof_operand env op =
  match op with
  | Reg reg -> typeof_reg env reg (* reg *)
  | Word word_val -> typeof_word env word_val 
  | _ -> Int (* TODO *)

and typeof_code (env : static_env) code =
  match code with
  | Code (type_assignments, reg_assignments, ins_seq) -> 
    let (l, r, _) = env in
    let _ = typeof_reg_asgn (l, r, type_assignments) reg_assignments in
    let _ = typeof_ins_seq (l, reg_assignments, type_assignments) ins_seq in
    Forall (type_assignments, reg_assignments)

let typecheck intputFile =
  let ast = parseFile intputFile in
  (* Enter from the `instruction_seq` of "_main" *)
  let main = lookup_code_block ast "_main" in
  let _ = typeof_code empty_env main in
  print_endline "Typechecked" ;
  ast