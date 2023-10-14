{
open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

(* Constant value start with $ *)
let immediate = '$' '-'? digit+

(* Labels start with _ *)
let label = '_' ['a'-'z' 'A'-'Z' '0'-'9']+

(* Stack type variables start with ! *)
let stack_type_var = '$' letter+

(* Comment start with ; *)
let comment = ';' [^'\n']+

(* Single line comment start with # *)
let sl_comment = '#' [^'\n']+

rule read =
  parse
  | white { read lexbuf }
  | '\n' { Lexing.new_line lexbuf; read lexbuf }
  (* Registers *)
  | "eax" { EAX }
  | "ebx" { EBX }
  | "ecx" { ECX }
  | "edx" { EDX }
  | "esi" { ESI }
  | "edi" { EDI }
  | "edp" { EDP }
  | "esp" { ESP }
  | "rax" { RAX }
  | "rbx" { RBX }
  | "rcx" { RCX }
  | "rdx" { RDX }
  | "rsi" { RSI }
  | "rdi" { RDI }
  | "rbp" { RBP }
  | "rsp" { RSP }
  (* Instructions *)
  | "jmp" { JMP }
  | "halt" { HALT }
  | "add" { ADD }
  | "sub" { SUB }
  | "mul" { MUL }
  | "beq" { BEQ }
  | "bneq" { BNEQ }
  | "bgt" { BGT }
  | "blt" { BLT }
  | "bgte" { BGTE }
  | "blte" { BLTE }
  | "mov" { MOV }
  | "st" { ST }
  | "ld" { LD }
  | "malloc" { MALLOC }
  | "unpack" { UNPACK }
  | "Forall" { FORALL }
  | "Exist" { EXIST }
  | "WordPack" { WORD_PACK }
  | "OperandPack" { OPERAND_PACK}
  | "as" { AS }
  | "code" { CODE }
  | "nil" { NIL }
  | "::" { CONS }
  | "@" { APPEND }
  | "PTR" { TPTR }
  | "cdot" { TY_ASGN_NIL }
  | "sp" { SP }
  | "<" { LTS }
  | ">" { GTS }
  | "[" { LSB }
  | "]" { RSB }
  | "{" { LCB }
  | "}" { RCB }
  | "." { DOT }
  | "," { COMMA }
  | ":" { COLON }
  | "(" { LPAREN }
  | ")" { RPAREN }
  (* Types *)
  | "int" { TINT }
  (* Misc *)
  (* | "\n" { NEWLINE } *)
  (* id, int, eof *)
  | label { LABEL (Lexing.lexeme lexbuf) }
  | id { TVAR (Lexing.lexeme lexbuf) }
  | stack_type_var { STVAR (Lexing.lexeme lexbuf) }
  | comment { COMMENT (Lexing.lexeme lexbuf) }
  | sl_comment { SINGLE_LINE_COMMENT (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | immediate { IMMEDIATE (let i = (Lexing.lexeme lexbuf) in 
    int_of_string (String.sub i 1 (String.length i - 1))) }
  | eof { EOF }
  | _ as c { failwith (Printf.sprintf "unexpected character: %C" c) }
  