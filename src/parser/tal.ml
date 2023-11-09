type name = string

type reg = Eax | Ebx | Ecx | Edx | Esi | Edi | Ebp | Esp | Rax | Rbx | Rcx | Rdx | Rsi | Rdi | Rbp | Rsp

(* Types *)

type kind = 
  | KLoc
  | KCap
  | KACap
  | KTy
  | KATy

type cap_var = CapVar of name
type loc_var = LocVar of name
type ty_var = TyVar of name
type aty_var = ATyVar of name
type alloc_var = AllocVar of name

and con_var =
  | CVLoc of loc_var
  | CVCap of cap_var
  | CVTy of ty_var
  | CVATy of aty_var
  | CVAlloc of alloc_var

and con =
  | ConLoc of location
  | ConCap of access_cap
  | ConTy of ty
  | ConATy of aty

(* TODO: Runtime location needed here? *)

and location =
  | LocCon of loc_var (* η *)
  | LocB (* b *)
  | LocNext of location

and predicate = 
  | PStacktop
  | Pstackbase
  | Ptuplestart
  | Ptupleend
  
and access_cap = 
  | CapCon of cap_var (* ε *)
  | CapNil
  | CapTy of location * ty
  | CapATy of location * aty
  | CapOTimes of access_cap * access_cap
  | CapWedge of access_cap * access_cap
  | CapFrac of access_cap * access_cap

and alloc_cap = alloc_cap_item list

and alloc_cap_item =
  | AllocCon of alloc_var
  | AllocPred of location * predicate

and ty = 
  | TyCon of ty_var (* Type Variable *)
  | TTop (* top type *)
  | Int
  | TyPtr of location
  | Exist of ty_asgn * access_cap * alloc_cap * ty
  | Forall of ty_asgn * reg_asgn * access_cap * alloc_cap

and aty =
  | ATyCon of aty_var
  | ATyNil
  | ATyCons of ty * aty

(* This is Γ *)
and reg_asgn = (reg_asgn_item) list

and reg_asgn_item = reg * ty

(* This is Δ *)
(* This should really be called context *)
and ty_asgn = ty_asgn_item list

and ty_asgn_item =
  | CtxtConVar of con_var (* FIXME: need kind here? *)
  | CtxtLoc of loc_var * location

(* This is ψ *)
and label_asgn = (name * ty) list

type rewrite = 
  | Drop
  | Comm
  | Assoc
  | Distr1
  | Distr2
  | Borrow
  | Return
  | Split
  | Join

and pos_ind = pos_ind_item list

and pos_ind_item =
  | Pos1
  | Pos2

and witness_item = rewrite * pos_ind

and witness = witness_item list

(* used to extract the items in a stack *)
type stack_item =
  | SITy of ty
  | SISty of aty

(* Values *)

type heap_val =
  | Words of (word_val) list
  | HCode of code

and code = 
  | Code of ty_asgn * reg_asgn * access_cap * alloc_cap * instruction_seq

and code_block =
  | CodeBlock of name * code

and word_val = 
  | Label of name (* #d *)
  | Immediate of int
  | WordIns of word_val * con list
  | Ptr
  | Ns
  (* pack? *)

and operand =
  | Reg of reg
  | Word of word_val
  | OperandIns of operand * con list  

(* Instructions *)

and code_block_seq = 
  | CodeBlockSeq of code_block
  | CodeBlockSeqCons of code_block * code_block_seq

and instruction_seq = 
  | Jmp of operand
  | Halt of ty
  | InstructionSeq of instruction_line * instruction_seq

and instruction_line =
  | InstructionLine of instruction * string option
  | Comment of string

and instruction = 
  | Aop of aop * reg * operand
  | Mov of reg * operand
  | Ld of reg * reg * int
  | St of reg * reg * int
  | Bop of bop * reg * operand
  | Malloc of (ty) list
  | Pack of con list * access_cap * alloc_cap * reg
  | Unpack of ty_asgn * reg
  | Salloc of int
  | Sfree of int
  | Nop 
  | MakeStack of loc_var * int
  | FreeStack
  | Coerce of witness 

and aop = 
  | Add | Sub | Mul

and bop = 
  | Beq | Bneq | Bgt | Blt | Bgte | Blte

