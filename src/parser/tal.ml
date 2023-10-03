type name = string

type reg = Eax | Ebx | Ecx | Edx | Esi | Edi | Ebp | Esp

type label = string

(* Types *)

type kind = 
    K4byte  		  (* describes types of 32-bit values *)
  | Ktype   		  (* describes types of all values *)
  | Kstack  		  (* describes types of the stack & pointers into it *)

type reg_asgn = (reg * ty) list

and ty_asgn = (label * ty) list

and label_asgn = (label * ty) list

and ty = 
  | Var of type_var
  | Int
  | Forall of ty_asgn * reg_asgn
  | Exist of name * kind * ty

and type_var =
  | TVar of name

(* Values *)

type heap_val =
  | Words of (word_val) list
  | Code of ty_asgn * reg_asgn * instruction

and word_val = 
  | Label of label
  | Immediate of int
  | WordPack of ty * word_val
(* Omitted polymorphic type instantiation *)

and operand =
  | Reg of reg
  | Word of word_val
  | OperandPack of ty * operand

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