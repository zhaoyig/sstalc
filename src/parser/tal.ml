type name = string

type reg = Eax | Ebx | Ecx | Edx | Esi | Edi | Ebp | Esp

type label = string

(* Types *)

type kind = 
    K4byte  		  (* describes types of 32-bit values *)
  | Ktype   		  (* describes types of all values *)
  | Kstack  		  (* describes types of the stack & pointers into it *)

type reg_asgn = RegAsgn of (reg_asgn_item) list

and reg_asgn_item = RegAsgnItem of reg * ty

and ty_asgn = TyAsgn of ty_asgn_item list

and ty_asgn_item = TyAsgnItem of type_var

and label_asgn = (label * ty) list

and ty = 
  | Var of type_var (* Type Variable *)
  | Int
  | TypeList of (ty) list
  | Forall of ty_asgn * reg_asgn
  | Exist of type_var * ty

and type_var =
  | TVar of name

(* Values *)

type heap_val =
  | Words of (word_val) list
  | Code of ty_asgn * reg_asgn * instruction

and word_val = 
  | Label of label
  | Immediate of int
  | WordPack of ty * word_val * ty
(* Omitted polymorphic type instantiation *)

and operand =
  | Reg of reg
  | Word of word_val
  | OperandPack of ty * operand * ty

(* Omitted polymorphic type instantiation *)

(* Instructions *)

and instruction_seq = 
  | Jmp of operand
  | Halt of ty
  | InstructionSeq of instruction * instruction_seq

and instruction = 
  | Aop of aop * reg * reg * operand
  | Mov of reg * operand
  | Ld of reg * reg * int
  | St of reg * reg * int
  | Bop of bop * reg * operand
  | Malloc of reg * (operand) list
  | Unpack of type_var * reg * operand

and aop = 
  | Add | Sub | Mul

and bop = 
  | Beq | Bneq | Bgt | Blt | Bgte | Blte