open Tal

type location =
  | Location of Lexing.position * Lexing.position (** delimited location *)
  | Nowhere (** no location *)

exception Error of (location * string * string)


let parse (s : string) : code_block_seq =
  let lexbuf = Lexing.from_string s in
  let code_block_seq = Parser.prog Lexer.read lexbuf in
  code_block_seq

let parseFile fn =
  try
    let fh = open_in fn in
    let lex = Lexing.from_channel fh in (* Create a lexbuf *)
    lex.Lexing.lex_curr_p <- {lex.Lexing.lex_curr_p with Lexing.pos_fname = fn};
    try
      let terms = Parser.prog Lexer.read lex in
      close_in fh;
      terms
    with
      (* Close the file in case of any parsing errors. *)
      Error err -> close_in fh ; raise (Error err)
  with
    (* Any errors when opening or closing a file are fatal. *)
    Error err -> () ; raise (Error err)