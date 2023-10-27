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
./factorial
```
## To run tests
```
dune runtest
```

## Note:
- `malloc` will overwrite `rax`
- substituion is not capture avoiding, name your type variables carefully

## Typing rule:
Entry is a code_block_seq
```
    env |- codeblock
-------------------------------
        env |- CodeBlockSeq codeblock
```

```
    env |- cb  env |- cbseq
-------------------------------
        env |- CodeBlockSeqCons cb cbseq
```

TODO:
- Use bdwgc for malloc
- Check if Bop is buggy when the registers are the same
- hval
- stack equality, correctness? (is serialization correct?)
- ~~forall type free variables?~~
- avoid captures