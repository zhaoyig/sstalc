## To parse from file:
`dune utop src`, then `Interp.Main.parse_file "path/to/file";;`. Path is relative to stalc folder.

## To compile a file
```bash
dune build
dune exec -- stalc test/factorial.stal -o test/factorial.asm
```

## To run .asm file on MacOS
```bash
nasm -f macho64 factorial.asm
ld -macosx_version_min 12.6.0 -L/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib -lSystem -o factorial factorial.o
```

## Note:
`malloc` will overwrite `rax`

TODO:
- Use bdwgc for malloc
- Check if Bop is buggy when the registers are the same