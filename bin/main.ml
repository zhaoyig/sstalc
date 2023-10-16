open Arg

let usageMsg = "stalc <file1> -o <output> -r "

let inputFile = ref ""
let outputFile = ref ""

let flagRun = ref false

let speclist = [
  ("-o", Set_string outputFile, "Set the output file name");
  ("-r", Set flagRun, "run the file");
]

let () = 
  Arg.parse speclist (fun file -> inputFile := file) usageMsg;
  Codegen.compileFile !inputFile !outputFile;