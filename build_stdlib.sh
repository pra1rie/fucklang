#!/bin/sh
set -xe

dmd -c -of=lib/core.o core/*.d
tcc -c -o lib/std.o lib/*.c -I core/
dmd -of=lib/stdio.so lib/*.o -shared -fPIC
