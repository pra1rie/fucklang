#!/bin/sh
set -xe

dmd -c -of=lib/core.o core/*.d
tcc -O2 -c -o lib/std.o lib/*.c -I core/
dmd -O -of=lib/stdio.so lib/*.o -shared -fPIC