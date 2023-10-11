%{
open Tal
open Stdlib
%}
// %token NEWLINE

%token <int> INT
%token <string> LABEL
%token <string> COMMENT
%token EAX
%token EBX
%token ECX
%token EDX
%token ESI
%token EDI
%token EDP
%token ESP
%token RAX
%token RBX
%token RCX
%token RDX
%token RSI
%token RDI
%token RBP
%token RSP

(* Instructions *)
%token JMP
%token HALT
%token ADD
%token SUB
%token MUL

%token BEQ 
%token BNEQ
%token BGT 
%token BLT 
%token BGTE
%token BLTE

%token MOV
%token LD
%token ST
%token MALLOC
%token UNPACK
%token WORD_PACK
%token OPERAND_PACK
%token AS
%token CODE

(* Types *)
%token <Tal.name> TVAR (* Type variable *)
%token <Tal.name> STVAR (* Stack type variable *)
%token TINT (* Type Int *)
%token FORALL 
%token EXIST
%token NIL
%token CONS
%token APPEND
%token TPTR (* Pointer type *)
%token SP
%token TY_ASGN_NIL

(* Special Chars *)
%token LTS (* less than sign *)
%token GTS
%token LSB (* left square bracket *)
%token RSB 
%token LCB (* left curly braces *)
%token RCB
%token DOT
%token COMMA
%token COLON
%token LPAREN
%token RPAREN
%token EOF

%left APPEND
%left CONS (* Should it be left assoc? *)
%start <Tal.code_block_seq> prog
%%

prog:
  | e = code_block_seq; EOF { e }

code_block_seq:
  | code_block { CodeBlockSeq $1 }
  | code_block; code_block_seq { CodeBlockSeqCons ($1, $2) }

instruction_seq:
  | JMP; x = operand { Jmp x }
  | HALT; x = ty { Halt x }
  | instruction_line; instruction_seq { InstructionSeq ($1, $2)}

code_block:
  | LABEL COLON code { CodeBlock ($1, $3) }

code:
  | CODE LSB t = ty_asgn RSB r = reg_asgn DOT is = instruction_seq { Code (t, r, is) }

instruction_line:
  | option(LABEL) option(COLON) instruction option(COMMENT) { InstructionLine ($1, $3, $4) }

instruction:
  | aop rd = reg COMMA rs = reg COMMA v = operand { Aop ($1, rd, rs, v) }
  | MOV rd = reg COMMA v = operand { Mov (rd, v) }
  | LD rd = reg COMMA rs = reg LPAREN i = INT RPAREN { Ld (rd, rs, i) }
  | ST rd = reg LPAREN i = INT RPAREN rs = reg { St (rd, rs, i) }
  | bop reg COMMA operand { Bop ($1, $2, $4) }
  | MALLOC r = reg COMMA LTS l = list(operand) GTS { Malloc (r, l) } 
  | UNPACK LSB a = TVAR r = reg RSB COMMA v = operand { Unpack ((TVar a), r, v)}

aop:
  | ADD { Add }
  | SUB { Sub }
  | MUL { Mul }

bop:
  | BEQ { Beq }
  | BNEQ { Bneq }
  | BGT { Bgt }
  | BLT { Blt }
  | BGTE { Bgte }
  | BLTE { Blte }

ty:
  | TVAR { Var (TVar $1) }
  | TINT { Int }
  | LTS l = list(ty) GTS { TypeList l }
  | FORALL LSB a = ty_asgn RSB DOT r = reg_asgn { Forall (a, r) }
  | EXIST TVAR DOT ty { Exist (TVar $2, $4) }
  | TPTR LPAREN stack_ty RPAREN { TPtr $3 }

stack_ty:
  | STVAR { StackTypeVar (STVar $1) }
  | NIL { Nil }
  | ty CONS stack_ty { Cons ($1, $3) }
  | stack_ty APPEND stack_ty { Append ($1, $3) }

ty_asgn:
  | TY_ASGN_NIL { TyAsgnNil }
  | STVAR COMMA ty_asgn { TyAsgnCons1 ((TVar $1), $3) }
  | TVAR COMMA ty_asgn { TyAsgnCons2 ((STVar $1), $3) }

reg_asgn:
  | LCB SP COLON st = stack_ty COMMA l = separated_list(COMMA, reg_asgn_item) RCB { RegAsgn (st, l) }
  | LCB SP COLON st = stack_ty RCB { RegAsgn (st, []) }

reg_asgn_item:
  | reg COLON ty { RegAsgnItem ($1, $3) }

word_val:
  | x = LABEL { Label x }
  | x = INT { Immediate x }
  | WORD_PACK LSB t = ty COMMA w = word_val RSB AS tprime = ty { WordPack (t, w, tprime)}

reg:
  | EAX { Eax }
  | EBX { Ebx }
  | ECX { Ecx }
  | EDX { Edx }
  | ESI { Esi }
  | EDI { Edi }
  | EDP { Ebp }
  | ESP { Esp }
  | RAX { Rax }
  | RBX { Rbx }
  | RCX { Rcx }
  | RDX { Rdx }
  | RSI { Rsi }
  | RDI { Rdi }
  | RBP { Rbp }
  | RSP { Rsp }

operand:
  | x = reg { Reg x }
  | x = word_val { Word x }
  | OPERAND_PACK LSB t = ty COMMA o = operand RSB AS tprime = ty { OperandPack (t, o, tprime)}
%%
