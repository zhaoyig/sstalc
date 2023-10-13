type name = string

type reg = Eax | Ebx | Ecx | Edx | Esi | Edi | Ebp | Esp | Rax | Rbx | Rcx | Rdx | Rsi | Rdi | Rbp | Rsp

type label = string

(* Types *)

type kind = 
    K4byte  		  (* describes types of 32-bit values *)
  | Ktype   		  (* describes types of all values *)
  | Kstack  		  (* describes types of the stack & pointers into it *)

type reg_asgn = RegAsgn of stack_ty * ((reg_asgn_item) list)

and reg_asgn_item = RegAsgnItem of reg * ty

and ty_asgn =
  | TyAsgnNil
  | TyAsgnCons1 of type_var * ty_asgn
  | TyAsgnCons2 of stack_type_var * ty_asgn

and label_asgn = (label * ty) list

and ty = 
  | Var of type_var (* Type Variable *)
  | Int
  | TypeList of (ty) list
  | Forall of ty_asgn * reg_asgn
  | Exist of type_var * ty
  | TPtr of stack_ty
  
and type_var =
  | TVar of name

and stack_ty = 
  | StackTypeVar of stack_type_var
  | Nil
  | Cons of ty * stack_ty
  | Append of stack_ty * stack_ty

and stack_type_var =
  | STVar of name

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
  | InstructionLine of label option * instruction * string option
  | Comment of string

and instruction = 
  | Aop of aop * reg * reg * operand
  | Mov of reg * operand
  | Ld of reg * reg * int
  | St of reg * reg * int
  | Bop of bop * reg * operand
  | Malloc of reg * (ty) list
  | Unpack of type_var * reg * operand

and aop = 
  | Add | Sub | Mul

and bop = 
  | Beq | Bneq | Bgt | Blt | Bgte | Blte

