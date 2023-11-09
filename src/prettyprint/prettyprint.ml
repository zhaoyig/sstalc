open Codegen
open TalParser.Tal
open Printf

let pp_reg = function
  | Eax -> "eax"
  | Ebx -> "ebx"
  | Ecx -> "ecx"
  | Edx -> "edx"
  | Esi -> "esi"
  | Edi -> "edi"
  | Ebp -> "ebp"
  | Esp -> "esp"
  | Rax -> "rax"
  | Rbx -> "rbx"
  | Rcx -> "rcx"
  | Rdx -> "rdx"
  | Rsi -> "rsi"
  | Rdi -> "rdi"
  | Rbp -> "rbp"
  | Rsp -> "rsp"

let rec pp_instruction = function
  | Aop (_, reg, op') ->
    formatInstruction "aop" [pp_reg reg; compileOperand op']
  | Mov (reg, op) ->
    formatInstruction "mov" [pp_reg reg; compileOperand op]
  | Ld (reg1, reg2, offset) ->
    formatInstruction "ld" [pp_reg reg1; pp_reg reg2; string_of_int offset]
  | St (reg1, reg2, offset) ->
    formatInstruction "st" [pp_reg reg1; pp_reg reg2; string_of_int offset]
  | Bop (_, reg, op') ->
    formatInstruction "bop" [pp_reg reg; compileOperand op']
  | Malloc tys ->
    formatInstruction "malloc" (List.map (fun x -> pp_ty x) tys)
  | Unpack (ta, reg) ->
    formatInstruction "unpack" [pp_ty_asgn ta; pp_reg reg]
  | Salloc n ->
    formatInstruction "salloc" [string_of_int n]
  | Sfree n ->
    formatInstruction "sfree" [string_of_int n]
  | Nop -> ""
  | MakeStack _ -> ""
  | _ -> failwith "TODO"

and pp_ty = function
  | Int -> "Int"
  | Forall (ta, ra, _, _) -> 
    Printf.sprintf "∀[%s].(%s,%s)" (pp_ty_asgn ta) (pp_reg_asgn ra) "TODO"
  | Exist (_, _, _, _) -> "Exist(TODO)"
  | TyCon _ -> "Type Var(TODO)"
  | TyPtr _ -> "Stack pointer(TODO)"
  | TTop -> "⊤"

and pp_word = function
  | Immediate i -> string_of_int i
  | Label l -> l
  | _ -> "TODO"

and pp_op = function
  | Reg r -> pp_reg r
  | Word w -> pp_word w
  | OperandIns (op, _) -> Printf.sprintf "%s[%s]" (pp_op op) "TODO"

and pp_sty = function 
  | ATyCon _ -> "TODO"
  | ATyCons (t, st) -> "(" ^ pp_ty t ^ " :: " ^ pp_sty st ^ ")"
  | ATyNil -> "nil" (* TODO *)

and pp_reg_asgn ra =
  "{" ^ String.concat ", " (List.map 
        (fun x -> (let (rr, tt) = x in
         (pp_reg rr) ^ " : " ^ (pp_ty tt)))
        ra) ^ "}"
  
and pp_ty_asgn ta =
  String.concat ", " (List.map (fun x -> 
    (match x with 
      | CtxtConVar (_) -> "c:kind"
      | CtxtLoc (_, _) -> "n = l")) ta)

let pp_label_asgn la =
  String.concat ", " (List.map (fun x -> let (n, t) = x in (n ^ ": " ^ (pp_ty t))) la)

let pp_env (env) = (* TODO *)
  let (_, r, _, _) = env in
  let reg_asgn_str = pp_reg_asgn r in
  Printf.sprintf "R: %s | TypeVar: %s | Cap: %s" reg_asgn_str "TODO" "TODO"

let pp_stack_list sl = 
  "[" ^ (String.concat "," (List.map (fun x -> (match x with 
    | SISty stv -> pp_sty stv
    | SITy tv -> pp_ty tv)) sl)) ^ "]"

let pp_loc_var lv =
  match lv with
  | LocVar s -> s

let rec pp_loc loc =
  match loc with
  | LocCon lv -> pp_loc_var lv
  | LocB -> "TODO"
  | LocNext l -> sprintf "next(%s)" (pp_loc l)
