{
open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

(* Labels start with letter l, to distinguish from type variables *)
let label = 'l' digit+

(* Stack type variables start with underscore *)
let stack_type_var = '_' letter+

rule read =
  parse
  | white { read lexbuf }
  (* Registers *)
  | "eax" { EAX }
  | "ebx" { EBX }
  | "ecx" { ECX }
  | "edx" { EDX }
  | "esi" { ESI }
  | "edi" { EDI }
  | "edp" { EDP }
  | "esp" { ESP }
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
  | "Unpack" { UNPACK }
  | "Forall" { FORALL }
  | "Exist" { EXIST }
  | "WordPack" { WORD_PACK }
  | "OperandPack" { OPERAND_PACK}
  | "as" { AS }
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
  | "\n" { NEWLINE }
  (* id, int, eof *)
  | label { LABEL (Lexing.lexeme lexbuf) }
  | id { TVAR (Lexing.lexeme lexbuf) }
  | stack_type_var { STVAR (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }
  | _ as c { failwith (Printf.sprintf "unexpected character: %C" c) }
  