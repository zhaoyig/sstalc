## To parse from file:
`dune utop src`, then `Interp.Main.parse_file "path/to/file";;`. Path is relative to stalc folder.

## Note:
`malloc` will overwrite `rax`

TODO:
- Use bdwgc for malloc
- 