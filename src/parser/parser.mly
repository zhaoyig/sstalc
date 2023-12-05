%{
open Tal
open Stdlib
%}
// %token NEWLINE

%token <int> INT
%token <int> IMMEDIATE
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
// %token WORD_PACK
%token OPERAND_PACK
%token AS
%token CODE
%token SALLOC
%token SFREE

(* Types *)
%token TINT (* Type Int *)
%token FORALL 
%token EXIST
%token NIL
%token CONS
%token APPEND
%token TPTR (* Pointer type *)
%token TTOP
%token NS
%token MAKESTACK

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
%token <string> SINGLE_LINE_COMMENT

(* Addition *)
%token OTIMES "⨷"
%token WEDGE "∧"
%token FRAC "/"
%token ACAPNIL "acap⋅"
%token ATYNIL "aty⋅"

%token DROP "Drop"
%token COMM "Comm"
%token ASSOC "Assoc"
%token DISTR1 "Distr1"
%token DISTR2 "Distr2"
%token BORROW "Borrow"
%token RETURN "Return"
%token SPLIT "Split"
%token JOIN "Join"

%token POS1 "p1"
%token POS2 "p2"
%token NEXT "next"
%token SEMICOLON ";"
%token CAPNIL "capnil"
%token PIPE "|"
%token EQ "="
%token FREESTACK "freestack"
%token COERCE "coerce"
%token PACK "pack"

%token <string> LOCVAR
%token <string> CAPVAR
%token <string> ACAPVAR
%token <string> TYVAR
%token <string> ATYVAR

%token STACKTOP "stacktop"
%token STACKBASE "stackbase"
%token TUPLESTART "tuplestart"
%token TUPLEEND "tupleend"

%token KLOC "Loc"
%token KCAP "Cap"
%token KACAP "ACap"
%token KTYPE "Type"
%token KATYPE "AType"
  
%left APPEND
%right CONS
%right OTIMES
%right WEDGE
%right FRAC
%start <Tal.code_block_seq> prog
%%

prog:
  | e = code_block_seq; EOF { e }

// dummy code to pass compile
// code_block_seq:
//   | EOF { CodeBlockSeq (CodeBlock ("", Code ([], [], CapNil, ACapNil, (Halt Int))))}
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
  | CODE LSB ta = ty_asgn RSB LPAREN ra = reg_asgn c = access_cap ksi = alloc_cap RPAREN is = instruction_seq { Code (ta, ra, c, ksi, is) }

instruction_line:
  | instruction option(COMMENT) { InstructionLine ($1, $2) }
  | SINGLE_LINE_COMMENT { Comment $1 }

instruction:
  | aop rd = reg COMMA v = operand { Aop ($1, rd, v) }
  | MOV rd = reg COMMA v = operand { Mov (rd, v) }
  | LD rd = reg COMMA LSB rs = reg COMMA i = INT RSB { Ld (rd, rs, i) }
  | ST LSB rd = reg COMMA i = INT RSB COMMA rs = reg { St (rd, rs, i) }
  | bop reg COMMA operand { Bop ($1, $2, $4) }
  | MALLOC LSB ita = loc_var RSB COMMA reg COMMA i = INT { Malloc (ita, i) } 
  | UNPACK LSB delta = ty_asgn RSB COMMA r = reg { Unpack (delta, r) }
  | SALLOC INT { Salloc $2 }
  | SFREE INT { Sfree $2 }
  | MAKESTACK loc_var INT { MakeStack ($2, $3) }
  | FREESTACK { FreeStack }
  | COERCE witness { Coerce $2 }
  | PACK LSB l1 = separated_list(COMMA, con) PIPE c = access_cap COMMA ksi = alloc_cap RSB COMMA r = reg AS l2 = separated_list(COMMA, con_var) { Pack (l1, c, ksi, r, l2) }

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

ty_var:
  | TYVAR { TyVar $1 }

aty_var:
  | ATYVAR { ATyVar $1 }

loc_var:
  | LOCVAR { LocVar $1 }

ty:
  | ty_var { TyCon $1 }
  | TINT { Int }
  | FORALL LSB a = ty_asgn RSB DOT LPAREN ra = reg_asgn COMMA c = access_cap COMMA ksi = alloc_cap RPAREN { Forall (a, ra, c, ksi) }
  | EXIST LSB ta = ty_asgn PIPE c = access_cap COMMA ksi = alloc_cap RSB DOT t = ty { Exist (ta, c, ksi, t) }
  | TPTR LPAREN location RPAREN { TyPtr $3 }
  | TTOP { TTop }

aty:
  | aty_var { ATyCon $1 }
  | ATYNIL { ATyNil }
  | ty CONS aty { ATyCons ($1, $3) }

ty_asgn:
  | separated_list(COMMA, ty_asgn_item) { $1 }

ty_asgn_item:
  | con_var { CtxtConVar $1 }
  | loc_var EQ location { CtxtLoc ($1, $3) }

reg_asgn:
  | LCB l = separated_list(COMMA, reg_asgn_item) RCB { l }

reg_asgn_item:
  | reg COLON ty { ($1, $3) }

word_val:
  | x = LABEL { Label x }
  | x = IMMEDIATE { Immediate x }
  // | WORD_PACK LSB t = ty COMMA w = word_val RSB AS tprime = ty { WordPack (t, w, tprime)}
  | NS { Ns }
  | word_val LSB con RSB { WordIns ($1, $3) }

kind:
  | KLOC { KLoc }
  | KCAP { KCap }
  | KACAP { KCap }
  | KTYPE { KTy }
  | KATYPE { KATy }

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
  | operand LSB con RSB { OperandIns ($1, $3) }

access_cap:
  | CAPVAR { CapCon (CapVar $1) }
  | CAPNIL { CapNil }
  | location COLON ty { CapTy ($1, $3) }
  | location COLON aty { CapATy ($1, $3) }
  | access_cap OTIMES access_cap { CapOTimes ($1, $3) }
  | access_cap WEDGE access_cap { CapWedge ($1, $3) }
  | access_cap FRAC access_cap { CapFrac ($1, $3) }

alloc_cap:
  | acap_var { ACapCon $1 }
  | ACAPNIL { ACapNil }
  | location COLON predicate { ACap ($1, $3) }

cap_var:
  | CAPVAR { CapVar $1 }

acap_var:
  | ACAPVAR { ACapVar $1 }

rewrite:
  | DROP { Drop }
  | COMM { Comm }
  | ASSOC { Assoc }
  | DISTR1 { Distr1 }
  | DISTR2 { Distr2 }
  | BORROW { Borrow }
  | RETURN { Return }
  | SPLIT { Split }
  | JOIN { Join }

pos_ind_item:
  | POS1 { 1 }
  | POS2 { 2 }

pos_ind:
  | l = separated_list(COMMA, pos_ind_item) { l }

witness_item:
  | LPAREN r = rewrite SEMICOLON pi = pos_ind RPAREN { (r, pi) }

witness:
  | separated_list(COMMA, witness_item) { $1 }

location:
  | loc_var { LocCon $1 }
  | NEXT LPAREN loc = location RPAREN { LocNext loc }

con:
  | location { ConLoc $1 }
  | access_cap { ConCap $1 }
  | ty { ConTy $1 }
  | aty { ConATy $1 }
  | alloc_cap { ConACap $1 }

con_var:
  | loc_var { CVLoc $1 }
  | cap_var { CVCap $1 }
  | ty_var { CVTy $1 }
  | aty_var { CVATy $1 }
  | acap_var { CVACap $1 }

predicate:
  | STACKTOP { PStacktop }
  | STACKBASE { PStackbase }
  | TUPLESTART { PTuplestart }
  | TUPLEEND { PTupleend }
%%
