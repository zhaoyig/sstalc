%{
open Tal
open Stdlib
%}
%token NEWLINE

%token <int> INT
%token <Tal.name> TVAR
%token JMP
%token <string> LABEL
%token TINT (* Type Int *)
%token HALT
%token EAX
%token EBX
%token ECX
%token EDX
%token ESI
%token EDI
%token EDP
%token ESP

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

%token LTS (* less than sign *)
%token GTS
%token LSB (* left square bracket *)
%token RSB 
%token EOF

%start <Tal.instruction_seq> prog
%%

prog:
  | e = instruction_seq; EOF { e }

instruction_seq:
  | JMP; x = operand { Jmp x }
  | HALT; x = ty { Halt x }
  | instruction NEWLINE instruction_seq { InstructionSeq ($1, $3)}

instruction:
  | aop reg reg operand { Aop ($1, $2, $3, $4) }
  | MOV reg operand { Mov ($2, $3) }
  | LD reg reg INT { Ld ($2, $3, $4) }
  | ST reg reg INT { St ($2, $3, $4) }
  | bop reg operand { Bop ($1, $2, $3) }
  | MALLOC r = reg LTS l = list(operand) GTS { Malloc (r, l) } 
  | UNPACK LSB a = TVAR r = reg RSB v = operand { Unpack ((TVar a), r, v)}
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

word_val:
  | x = LABEL {Label x}
  | x = INT {Immediate x}

reg:
  | EAX {Eax}
  | EBX {Ebx}
  | ECX {Ecx}
  | EDX {Edx}
  | ESI {Esi}
  | EDI {Edi}
  | EDP {Ebp}
  | ESP {Esp}

operand:
  | x = reg {Reg x}
  | x = word_val {Word x}

%%
