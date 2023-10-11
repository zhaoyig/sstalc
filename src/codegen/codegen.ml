open TalParser.Main
open TalParser.Tal

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
  
let rec compileWordVal = function
  | Label l -> l
  | Immediate i -> string_of_int i
  | WordPack (_, wordVal, _) -> compileWordVal wordVal
  
let rec compileOperand = function
  | Reg r -> compileReg r
  | Word w -> compileWordVal w
  | OperandPack (_, op, _) -> compileOperand op

let compileAop = function
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "imul"

let compileOffset offset =
  (if offset < 0 then "-" else "+") ^ string_of_int offset

let compileBop = function
  | Beq -> "je"
  | Bneq -> "jne"
  | Bgt -> "jg"
  | Blt -> "jl"
  | Bgte -> "jge"
  | Blte -> "jle"

let compileInstruction = function
  | Aop (aop, reg1, reg2, operand) -> 
    let reg1Exp = compileReg reg1 in
    let reg2Exp = compileReg reg2 in
    let opExp = compileOperand operand in
    let aopExp = compileAop aop in
    [
      formatInstruction "mov" [reg1Exp; reg2Exp];
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
  | Malloc (_, _) ->
    [] (* TODO *)
  | Unpack (_, reg, operand) ->
    [
      formatInstruction "mov" [compileReg reg; compileOperand operand];
    ]

let rec compileInstructionSeq = function
  | Jmp op -> [compileOperand op]
  | Halt _ -> [
      "mov rax, 0x02000001";
      "syscall"
    ] (* MacOS exit syscall *)
  | InstructionSeq (instruction, instructionSeq) -> 
      compileInstruction instruction @ compileInstructionSeq instructionSeq

let compileFile filename = 
  let instruction_seq = parseFile filename in
  let compileResult = String.concat "\n"
    (compileInstructionSeq instruction_seq) in
  let oc = open_out (filename ^ ".asm") in
  Printf.fprintf oc "%s\n" compileResult;
  close_out oc
  