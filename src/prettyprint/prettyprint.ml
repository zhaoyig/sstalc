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
  | Malloc (loc_var, int) ->
    formatInstruction "malloc" [pp_loc_var loc_var; string_of_int int]
  | Unpack (ta, reg) ->
    formatInstruction "unpack" [pp_ty_asgn ta; pp_reg reg]
  | Salloc n ->
    formatInstruction "salloc" [string_of_int n]
  | Sfree n ->
    formatInstruction "sfree" [string_of_int n]
  | Nop -> "nop"
  | MakeStack (_, _) -> "makestack"
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

and pp_aty = function 
  | ATyCon _ -> "TODO"
  | ATyCons (t, st) -> "(" ^ pp_ty t ^ " :: " ^ pp_aty st ^ ")"
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

and pp_label_asgn la =
  String.concat ", " (List.map (fun x -> let (n, t) = x in (n ^ ": " ^ (pp_ty t))) la)

and pp_env (env) = (* TODO *)
  let (_, r, _, cap, _) = env in
  let reg_asgn_str = pp_reg_asgn r in
  Printf.sprintf "R: %s | TypeVar: %s | Access Cap: %s" reg_asgn_str "TODO" (pp_cap cap)

and pp_loc loc =
  match loc with
  | LocCon lv -> pp_loc_var lv
  | LocB -> "TODO"
  | LocNext l -> sprintf "next(%s)" (pp_loc l)

and pp_con c = 
  match c with
  | ConACap acap -> pp_acap acap
  | ConATy aty -> pp_aty aty
  | ConTy ty -> pp_ty ty
  | ConCap c -> pp_cap c
  | ConLoc l -> pp_loc l

and pp_acap _ =
  "TODO"

and pp_cap = function 
  | CapATy (loc, aty) -> sprintf "CAP(%s:%s)" (pp_loc loc) (pp_aty aty)
  | CapCon cap_var -> pp_cap_var cap_var
  | CapNil -> "⋅"
  | CapTy (loc, ty) -> sprintf "CAP(%s:%s)" (pp_loc loc) (pp_ty ty)
  | CapOTimes (c1, c2) -> sprintf "(%s⊗%s)" (pp_cap c1) (pp_cap c2)
  | CapWedge (c1, c2) -> sprintf "(%s∧%s)" (pp_cap c1) (pp_cap c2)
  | CapFrac (c1, c2) -> sprintf "(%s/%s)" (pp_cap c1) (pp_cap c2)

and pp_cap_var (cv : cap_var) : string =
  match cv with
  | CapVar name -> "CapVar " ^ name

and pp_loc_var (lv : loc_var) : string =
  match lv with
  | LocVar name -> "LocVar " ^ name

and pp_ty_var (tv : ty_var) : string =
  match tv with
  | TyVar name -> "TyVar " ^ name

and pp_aty_var (av : aty_var) : string =
  match av with
  | ATyVar name -> "ATyVar " ^ name

and pp_acap_var (acv : acap_var) : string =
  match acv with
  | ACapVar name -> "ACapVar " ^ name

let pp_con_var (cv : con_var) : string =
  match cv with
  | CVLoc loc_var -> "CVLoc " ^ pp_loc_var loc_var
  | CVCap cap_var -> "CVCap " ^ pp_cap_var cap_var
  | CVTy ty_var -> "CVTy " ^ pp_ty_var ty_var
  | CVATy aty_var -> "CVATy " ^ pp_aty_var aty_var
  | CVACap acap_var -> "CVACap " ^ pp_acap_var acap_var

let pp_kind (k : kind) : string =
  match k with
  | KLoc -> "KLoc"
  | KCap -> "KCap"
  | KACap -> "KACap"
  | KTy -> "KTy"
  | KATy -> "KATy"