open TalParser.Main
open TalParser.Tal

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

let lookup_label (env: static_env) x =
  let (label_assignment, _, _) = env in
  match List.assoc_opt x label_assignment with
  | Some t -> t
  | None -> failwith ("Unbound label: " ^ x)

(* TODO: Avoid duplication? *)
let extend_label label_assignment label_name typ =
  (label_name, typ) :: label_assignment

(* Convert individual reg_asgn_item to kv pair *)
let reg_asgn_item_to_pair (x : reg_asgn_item) =
  match x with
  | RegAsgnItem (r, t) -> (r, t) 

(* Inverse of ^ *)
let pair_to_reg_asgn_item (x : (reg * ty)) =
  let (r, t) = x in
  RegAsgnItem (r, t)
  
let lookup_register (env : static_env) (r : reg) : ty =
  let (_, (_, rf), _) = env in
  let rf_assoc = List.map reg_asgn_item_to_pair rf in
  List.assoc r rf_assoc

(* Update all elements with key `k` to have value `v` in a assoc list `l` *)
let rec update_assoc_list k v l =
  match l with
  | [] -> []
  | h :: t -> 
    let (kk, _) = h in
    if kk == k then (kk, v) :: update_assoc_list k v t
    else h :: update_assoc_list k v t

(* Return a new register assignment with `reg` having type `t` *)
let update_register_asgn (register_asgn : reg_asgn) (r : reg) (t : ty) =
  let (sp_type, normal_reg) = register_asgn in
  (sp_type, List.map pair_to_reg_asgn_item 
    (update_assoc_list r t (List.map reg_asgn_item_to_pair normal_reg)))

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
    let _ = (match h with
      | RegAsgnItem (_, typ) -> typeof_ty env typ) in
    typeof_each_rf env t

let typeof_stack_ty _ _ =
  ()

(* rftype *)
let typeof_reg_asgn env reg_assignments =
  let (stack_type, _) = reg_assignments in
  let _ = typeof_stack_ty env stack_type in  
  () (* TODO *)

(* Typecheck if register assignment ra1 is subtype of ra2 *)
let typeof_subtype _ _ _ =
  ()

(* seq, jmp, halt *)
let rec typeof_ins_seq _ ins_seq = 
  match ins_seq with
  | Jmp _ ->
      ()
  | Halt _ ->
      ()
  | InstructionSeq (_, _) ->
      ()

and typeof_instruction (env : static_env) ins =
  match ins with
  | Mov (reg, op) -> (* mov *)
    let (l, r, t) = env in
    let op_type = typeof_operand env op in
    let new_env = (l, update_register_asgn r reg op_type, t) in
    (new_env, ())
  | _ -> (env, ())

(* reg *)
and typeof_reg env reg =
  lookup_register env reg

and typeof_operand env op =
  match op with
  | Reg reg -> typeof_reg env reg
  | _ -> Int

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
  ast