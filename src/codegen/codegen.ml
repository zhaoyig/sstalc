open Ast

let compileReg = function
  | Eax -> "eax"
  | Ebx -> "ebx"
  | Ecx -> "ecx"
  | Edx -> "edx"
  | Esi -> "esi"
  | Edi -> "edi"
  | Ebp -> "ebp"
  | Esp -> "esp"
  | Rax -> "rax"
  | Rbx -> "rbx"
  | Rcx -> "rcx"
  | Rdx -> "rdx"
  | Rsi -> "rsi"
  | Rdi -> "rdi"
  | Rbp -> "rbp"
  | Rsp -> "rsp"

let formatInstruction instruction operands =
  instruction ^ " " ^ String.concat ", " operands

let compileAdress = function
| Address i -> string_of_int i

let compileLabel = function
  | LStr s -> s
  | LAdr adr -> compileAdress adr

let rec compileWordVal = function
  | Label l -> compileLabel l
  | Immediate i -> string_of_int i
  | WordPack (_, wordVal, _) -> compileWordVal wordVal
  | Ptr adr -> compileAdress adr
  | Ns -> "0"
  | WordSTyPoly (w, _) -> compileWordVal w
  | WordTyPoly (w, _) -> compileWordVal w

let rec compileOperand = function
  | Reg r -> compileReg r
  | Word w -> compileWordVal w
  | OperandPack (_, op, _) -> compileOperand op
  | OperandSTyPoly (op, _) -> compileOperand op
  | OperandTyPoly (op, _) -> compileOperand op

let compileAop = function
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "imul"

let compileOffset offset =
  (if offset < 0 then "-" else "+") ^ string_of_int ((abs offset) * 8)

let compileBop = function
  | Beq -> "je"
  | Bneq -> "jne"
  | Bgt -> "jg"
  | Blt -> "jl"
  | Bgte -> "jge"
  | Blte -> "jle"

let rec compileInstruction = function
  | Aop (aop, reg1, operand) -> 
    let reg1Exp = compileReg reg1 in
    let opExp = compileOperand operand in
    let aopExp = compileAop aop in
    [
      formatInstruction aopExp [reg1Exp; opExp];
    ]
  | Mov (reg1, operand) -> 
    [
      formatInstruction "mov" [compileReg reg1; compileOperand operand]
    ]
  | Ld (reg1, reg2, offset) ->
    [
      formatInstruction "mov" [compileReg reg1; "[" ^ compileReg reg2 ^ compileOffset offset ^ "]"]
    ]
  | St (reg1, reg2, offset) ->
    [
      formatInstruction "mov" ["[" ^ compileReg reg1 ^ compileOffset offset ^ "]"; compileReg reg2]
    ]
  | Bop (bop, reg, operand) ->
    [
      formatInstruction "cmp" [compileReg reg; "0"];
      formatInstruction (compileBop bop) [compileOperand operand];
    ]
  | Malloc tyList ->
    let len = List.length tyList in 
    let bytesToAlloc = len * 8 in [
      formatInstruction "mov" ["rdi"; string_of_int bytesToAlloc];
      formatInstruction "call" ["_malloc"];
    ]
  | Unpack (_, reg, operand) ->
    [
      formatInstruction "mov" [compileReg reg; compileOperand operand];
    ]
  | Salloc size -> [
    formatInstruction "sub" ["rsp"; string_of_int (size * 8)];
  ]
  | Sfree size -> [
    formatInstruction "add" ["rsp"; string_of_int (size * 8)]
  ]
  | Movsp1 (_, reg) -> [
    formatInstruction "mov" ["rsp"; compileReg reg]
  ]
  | Movsp2 (reg, _) -> [
    formatInstruction "mov" [compileReg reg; "rsp"]
  ]
  | Sst (reg1, reg2, offset) -> compileInstruction (St (reg1, reg2, offset))
  | Sld (reg1, reg2, offset) -> compileInstruction (Ld (reg1, reg2, offset))
  | Sstsp (_, r, offset) -> [
    formatInstruction "mov" ["[" ^ "rsp" ^ compileOffset offset ^ "]"; compileReg r]
  ]
  | Sldsp (r, _, offset) -> [
    formatInstruction "mov" [compileReg r; "[" ^ "rsp" ^ compileOffset offset ^ "]"; ]
  ]
  | Nop -> []
  | MakeStack wordsToAlloc -> 
    let bytesToAlloc = wordsToAlloc * 8 in [
    formatInstruction "mov" ["rdi"; string_of_int bytesToAlloc];
    formatInstruction "call" ["_malloc"];
    formatInstruction "add" ["rax"; string_of_int bytesToAlloc];
    formatInstruction "mov" ["rsp"; "rax"]
  ]


let compileInstructionLine = function
  | InstructionLine (instruction, comment) ->
    let commentLine = if Option.is_some comment then [ Option.get(comment) ] else [] in
    compileInstruction instruction @ commentLine
  | Comment comment -> [ comment ]

let rec compileInstructionSeq = function
  | Jmp op -> ["jmp " ^ compileOperand op]
  | Halt _ -> [
      "mov rax, 0x02000001";
      "syscall"
    ] (* MacOS exit syscall *)
  | InstructionSeq (instruction_line, instructionSeq) -> 
      compileInstructionLine instruction_line @ compileInstructionSeq instructionSeq

let compileCode = function
  | Code (_, _, ins_seq) ->
    compileInstructionSeq ins_seq

let compileCodeBlock = function
  | CodeBlock (label, code) -> 
    [(compileLabel label) ^ ":"] @ compileCode code

let rec compileCodeBlockSeq = function
  | CodeBlockSeq code_block -> compileCodeBlock code_block
  | CodeBlockSeqCons (code_block, code_block_seq) -> 
      compileCodeBlock code_block @ compileCodeBlockSeq code_block_seq

let compile_ast ast : string =
  let compiledInstructions = String.concat "\n"
    (compileCodeBlockSeq ast) in
  let header = String.concat "\n" [
    "global  _main";
    "section  .text";
    "extern _malloc"
  ]  ^ "\n" in
  let compiledResult = header ^ compiledInstructions in
  compiledResult
