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

rule read =
  parse
  | white { read lexbuf }
  (* Registers *)
  | "EAX" { EAX }
  | "EBX" { EBX }
  | "ECX" { ECX }
  | "EDX" { EDX }
  | "ESI" { ESI }
  | "EDI" { EDI }
  | "EDP" { EDP }
  | "ESP" { ESP }
  (* Instructions *)
  | "JMP" { JMP }
  | "HALT" { HALT }
  | "ADD" { ADD }
  | "SUB" { SUB }
  | "MUL" { MUL }
  | "BEQ" { BEQ }
  | "BNEQ" { BNEQ }
  | "BGT" { BGT }
  | "BLT" { BLT }
  | "BGTE" { BGTE }
  | "BLTE" { BLTE }
  (* Types *)
  | "int" { TINT }
  (* Misc *)
  | ";;" { NEWLINE }
  (* id, int, eof *)
  | label { LABEL (Lexing.lexeme lexbuf) }
  | id { TVAR (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }
  | _ as c { failwith (Printf.sprintf "unexpected character: %C" c) }
  