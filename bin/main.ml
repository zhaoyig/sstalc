open Arg
open Codegen
open SstalParser.Main

let usageMsg = "stalc <file1> -o <output> -r "

let inputFile = ref ""
let outputFile = ref ""

let flagRun = ref false

let speclist = [
  ("-o", Set_string outputFile, "Set the output file name");
  ("-r", Set flagRun, "run the file");
]

let compileFile inputFilename outputFilename = 
  let ins_seq_seq = parseFile inputFilename in
  let compiledInstructions = String.concat "\n"
    (compileCodeBlockSeq ins_seq_seq) in
  let header = String.concat "\n" [
    "global  _main";
    "section  .text";
    "extern _malloc"
  ]  ^ "\n" in
  let compiledResult = header ^ compiledInstructions in
  let oc = open_out (outputFilename) in
  Printf.fprintf oc "%s\n" compiledResult;
  close_out oc

let () = 
  Arg.parse speclist (fun file -> inputFile := file) usageMsg;
  ignore(Typecheck.typecheck !inputFile);
  compileFile !inputFile !outputFile;