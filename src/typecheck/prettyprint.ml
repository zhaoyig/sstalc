open Codegen
open TalParser.Tal

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

let pp_instruction = function
  | Mov (reg, op) -> formatInstruction "mov" [pp_reg reg; compileOperand op]
  | _ -> ""

let rec pp_ty = function
  | Int -> "Int"
  | TypeList l -> "<" ^ String.concat "," (List.map pp_ty l) ^ ">"
  | Forall (_, _) -> "Forall(TODO)"
  | Exist (_, _) -> "Exist(TODO)"
  | Var _ -> "Type Var(TODO)"
  | TPtr _ -> "Stack pointer(TODO)"
  | TTop -> "⊤"

let pp_word = function
  | Immediate i -> string_of_int i
  | Label l -> (match l with 
    | LAdr i -> "label: " ^ (match i with | Address i -> (string_of_int i))
    | LStr s -> "label: " ^ s)
  | _ -> "TODO"

let pp_op = function
  | Reg r -> pp_reg r
  | Word w -> pp_word w
  | _ -> "TODO"

let rec pp_sty = function
  | StackTypeVar v -> "stackVar: " ^ (match v with STVar s -> s)
  | Append (st1, st2) -> "(" ^ (pp_sty st1) ^ " @ " ^ (pp_sty st2) ^ ")"
  | Cons (t, st) -> "(" ^ pp_ty t ^ " :: " ^ pp_sty st ^ ")"
  | Nil -> "nil" (* TODO *)

let pp_reg_asgn ra =
  let (stack, normal_reg) = ra in
  (pp_sty stack) ^ ", "
    ^ String.concat ", " (List.map 
        (fun x -> (let (rr, tt) = x in
         (pp_reg rr) ^ " : " ^ (pp_ty tt) ))
        normal_reg)
  
let pp_ty_asgn ta =
  String.concat ", " (List.map (fun x -> (match x with | TAITVar v -> (match v with | TVar s -> ("TVar(" ^ s ^ ")")) | TAISTVar v -> (match v with | STVar s -> ("STVar(" ^ s ^ ")")))) ta)

let pp_env (env) = (* TODO *)
  let (_, r, _) = env in
  let reg_asgn_str = pp_reg_asgn r in
  Printf.sprintf "H: %s | R: %s | TypeVar: %s" "TODO" reg_asgn_str "TODO"