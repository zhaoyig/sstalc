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

let extend_label label_assignment label_name typ =
  (label_name, typ) :: label_assignment

and typeof_code (env : static_env) code =
    match code with
    | Code (type_assignments, reg_assignments, ins_seq) -> 
      let _ = typeof_ty_asgn env type_assignments in
      let _ = typeof_reg_asgn env reg_assignments in
      Forall (type_assignments, reg_assignments)

let typecheck intputFile =
  let ast = parseFile intputFile in
  (* Enter from the `instruction_seq` of "_main" *)
  let main = lookup_code_block ast "_main" in
  let _ = typeof_code empty_env main in
  ast