#!/bin/sh
set -xe

dmd -c -of=lib/core.o core/*.d src/*.d -I=src/
gcc -O3 -c -o lib/std.o lib/*.c -I core/
dmd -O -of=lib/stdlib.so lib/*.o -shared -fPIC
