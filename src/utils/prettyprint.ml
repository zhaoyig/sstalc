open Codegen
open TalParser.Tal

let pp_instruction = function
  | Mov (reg, op) -> formatInstruction "mov" [compileReg reg; compileOperand op]
  | _ -> ""