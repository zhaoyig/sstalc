#!/bin/bash
nasm -f macho64 $1.asm
ld -macosx_version_min 12.6.0 -L/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib -lSystem -o $1.exe $1.o
./$1.exe
echo $?
