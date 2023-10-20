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

let pp_op = function
  | Reg r -> pp_reg r
  | Word _ -> "Word(TODO)"
  | _ -> "TODO"

let pp_stack_ty = function
  | StackTypeVar _ -> "stack???"
  | _ -> "" (* TODO *)

let pp_env (env) = (* TODO *)
  let (_, r, _) = env in
  let (stack, normal_reg) = r in
  let r_str = (pp_stack_ty stack)
    ^ String.concat "," (List.map 
        (fun x -> (let (rr, tt) = x in
         (pp_reg rr) ^ (pp_ty tt) ))
        normal_reg) in
  Printf.sprintf "H: %s | R: %s | TypeVar: %s" "TODO" r_str "TODO"