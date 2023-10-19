type name = string

type reg = Eax | Ebx | Ecx | Edx | Esi | Edi | Ebp | Esp | Rax | Rbx | Rcx | Rdx | Rsi | Rdi | Rbp | Rsp

type sp = Sp

type label = 
  | LStr of string
  | LAdr of address

and address = 
  | Address of int

(* Types *)

type kind = 
    K4byte  		  (* describes types of 32-bit values *)
  | Ktype   		  (* describes types of all values *)
  | Kstack  		  (* describes types of the stack & pointers into it *)

(* This is Γ *)
type reg_asgn = stack_ty * ((reg_asgn_item) list)

and reg_asgn_item = RegAsgnItem of reg * ty

(* This is Δ *)
and ty_asgn =
  | TyAsgnNil
  | TyAsgnCons1 of type_var * ty_asgn
  | TyAsgnCons2 of stack_type_var * ty_asgn

and ty = 
  | Var of type_var (* Type Variable *)
  | Int
  | TypeList of (ty) list
  | Forall of ty_asgn * reg_asgn
  | Exist of type_var * ty
  | TPtr of stack_ty
  
and type_var =
  | TVar of name

(* This is σ *)
and stack_ty = 
  | StackTypeVar of stack_type_var
  | Nil
  | Cons of ty * stack_ty
  | Append of stack_ty * stack_ty

and stack_type_var =
  | STVar of name

(* This is ψ *)
and label_asgn = (name * ty) list

(* Values *)

type heap_val =
  | Words of (word_val) list
  | HCode of code

and code = 
  | Code of ty_asgn * reg_asgn * instruction_seq

and code_block =
  | CodeBlock of label * code

and word_val = 
  | Label of label
  | Immediate of int
  | WordPack of ty * word_val * ty
(* Omitted polymorphic type instantiation *)
  | Ptr of address

and operand =
  | Reg of reg
  | Word of word_val
  | OperandPack of ty * operand * ty

(* Omitted polymorphic type instantiation *)

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
  | Unpack of type_var * reg * operand
  | Salloc of int
  | Sfree of int
  | Movsp1 of sp * reg (* Mov into sp *)
  | Movsp2 of reg * sp (* Mov out of sp *)
  | Sst of reg * reg * int
  | Sld of reg * reg * int
  | Sstsp of sp * reg * int
  | Sldsp of reg * sp * int
  | Nop 

and aop = 
  | Add | Sub | Mul

and bop = 
  | Beq | Bneq | Bgt | Blt | Bgte | Blte

