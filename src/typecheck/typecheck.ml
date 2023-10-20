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

let empty_env = ([], (Nil, []), TyAsgnNil)

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

(* type *)
let rec typeof_ty _ _ =
  (* let free_vars = get_free_vars typ in
  let unbound_free_vars = subset_of env free_vars in *)
  () (* TODO *)

(* Iterate through register fiheck each type *)
and typeof_each_rf env (l : (reg_asgn_item) list) =  
  match l with
  | [] -> ()
  | h :: t -> 
    let (_, typ) = h in
    let _ = typeof_ty env typ in
    typeof_each_rf env t

let typeof_stack_ty _ _ =
  ()

(* rftype *)
let typeof_reg_asgn env reg_assignments =
  let (stack_type, _) = reg_assignments in
  let _ = typeof_stack_ty env stack_type in  
  () (* TODO *)

(* Typecheck if register assignment ra1 is subtype of ra2, i.e. ra2 is a subset of ra1 *)
let typeof_subtype _ _ _ =
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