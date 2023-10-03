open Tal

let parse (s : string) : instruction_seq =
  let lexbuf = Lexing.from_string s in
  let instruction_seq = Parser.prog Lexer.read lexbuf in
  instruction_seq