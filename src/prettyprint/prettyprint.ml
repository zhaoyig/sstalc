open Codegen
open Ast

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
  | Unpack (type_var, reg, op) ->
    formatInstruction "unpack" [pp_ty (Var type_var); pp_reg reg; compileOperand op]
  | Salloc n ->
    formatInstruction "salloc" [string_of_int n]
  | Sfree n ->
    formatInstruction "sfree" [string_of_int n]
  | Movsp1 (_, reg) ->
    formatInstruction "mov" ["sp"; pp_reg reg]
  | Movsp2 (reg, _) ->
    formatInstruction "mov" [pp_reg reg; "sp"]
  | Sst (reg1, reg2, offset) ->
    formatInstruction "sst" [pp_reg reg1; pp_reg reg2; string_of_int offset]
  | Sld (reg1, reg2, offset) ->
    formatInstruction "sld" [pp_reg reg1; pp_reg reg2; string_of_int offset]
  | Sstsp (_, reg, offset) ->
    formatInstruction "sst" ["sp"; pp_reg reg; string_of_int offset]
  | Sldsp (reg, _, offset) ->
    formatInstruction "sld" [pp_reg reg; "sp"; string_of_int offset]
  | Nop -> ""
  | MakeStack _ -> ""

and pp_ty = function
  | Int -> "Int"
  | TypeList l -> "<" ^ String.concat "," (List.map pp_ty l) ^ ">"
  | Forall (ta, ra) -> 
    Printf.sprintf "∀[%s].%s" (pp_ty_asgn ta) (pp_reg_asgn ra)
  | Exist (_, _) -> "Exist(TODO)"
  | Var _ -> "Type Var(TODO)"
  | TPtr _ -> "Stack pointer(TODO)"
  | TTop -> "⊤"

and pp_word = function
  | Immediate i -> string_of_int i
  | Label l -> (match l with 
    | LAdr i -> "label: " ^ (match i with | Address i -> (string_of_int i))
    | LStr s -> s)
  | WordTyPoly (w, ty) -> Printf.sprintf "%s[%s]" (pp_word w) (pp_ty ty)
  | WordSTyPoly (w, sty) -> Printf.sprintf "%s[%s]" (pp_word w) (pp_sty sty)
  | _ -> "TODO"

and pp_op = function
  | Reg r -> pp_reg r
  | Word w -> pp_word w
  | OperandTyPoly (op, typ) -> Printf.sprintf "%s[%s]" (pp_op op) (pp_ty typ)
  | OperandSTyPoly (op, sty) -> Printf.sprintf "%s[%s]" (pp_op op) (pp_sty sty)
  | _ -> "TODO"

and pp_sty = function
  | StackTypeVar v -> (match v with STVar s -> s)
  | Append (st1, st2) -> "(" ^ (pp_sty st1) ^ " @ " ^ (pp_sty st2) ^ ")"
  | Cons (t, st) -> "(" ^ pp_ty t ^ " :: " ^ pp_sty st ^ ")"
  | Nil -> "nil" (* TODO *)

and pp_reg_asgn ra =
  let (stack, normal_reg) = ra in
  "{" ^ "sp: " ^ (pp_sty stack) ^ ", "
    ^ String.concat ", " (List.map 
        (fun x -> (let (rr, tt) = x in
         (pp_reg rr) ^ " : " ^ (pp_ty tt)))
        normal_reg) ^ "}"
  
and pp_ty_asgn ta =
  String.concat ", " (List.map (fun x -> (match x with | TAITVar v -> (match v with | TVar s -> ("TVar(" ^ s ^ ")")) | TAISTVar v -> (match v with | STVar s -> (s)))) ta)

let pp_label_asgn la =
  String.concat ", " (List.map (fun x -> let (n, t) = x in (n ^ ": " ^ (pp_ty t))) la)

let pp_env (env) = (* TODO *)
  let (_, r, _) = env in
  let reg_asgn_str = pp_reg_asgn r in
  Printf.sprintf "R: %s | TypeVar: %s" reg_asgn_str "TODO"

let pp_stack_list sl = 
  "[" ^ (String.concat "," (List.map (fun x -> (match x with 
    | SISty stv -> pp_sty stv
    | SITy tv -> pp_ty tv)) sl)) ^ "]"