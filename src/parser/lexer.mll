{
open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id_chars = ['a'-'z' 'A'-'Z' '_' '0'-'9']
let id = id_chars+

(* Constant value start with $ *)
let immediate = '$' '-'? digit+

(* Labels start with _ *)
let label = '_' ['a'-'z' 'A'-'Z' '0'-'9']+

(* Comment start with ; *)
let comment = ';' [^'\n']+

(* Single line comment start with # *)
let sl_comment = '#' [^'\n']+

let ty_var = id_chars
let aty_var = '!' id_chars
let cap_var = '`' id_chars
let acap_var = '~' id_chars
let loc_var = '*' id_chars

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
  (* | "WordPack" { WORD_PACK } *)
  | "OperandPack" { OPERAND_PACK}
  | "makestack" { MAKESTACK }
  | "sfree" { SFREE }
  | "salloc" { SALLOC }
  | "as" { AS }
  | "code" { CODE }
  | "nil" { NIL }
  | "::" { CONS }
  | "@" { APPEND }
  | "PTR" { TPTR }
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
  | "|" { PIPE }
  (* Types *)
  | "int" { TINT }
  | "top" { TTOP }
  | "ns" { NS }
  (* Misc *)
  (* | "\n" { NEWLINE } *)
  (* id, int, eof *)
  | label { LABEL (Lexing.lexeme lexbuf) }
  | ty_var { TYVAR (Lexing.lexeme lexbuf) }
  | aty_var { ATYVAR (Lexing.lexeme lexbuf) }
  | loc_var { LOCVAR (Lexing.lexeme lexbuf) }
  | cap_var { CAPVAR (Lexing.lexeme lexbuf) }
  | acap_var { ACAPVAR (Lexing.lexeme lexbuf) }
  | comment { COMMENT (Lexing.lexeme lexbuf) }
  | sl_comment { SINGLE_LINE_COMMENT (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | immediate { IMMEDIATE (let i = (Lexing.lexeme lexbuf) in 
    int_of_string (String.sub i 1 (String.length i - 1))) }
  | eof { EOF }
  | _ as c { failwith (Printf.sprintf "unexpected character: %C" c) }

  