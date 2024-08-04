# FuckLang
dumb little scripting language written in D.
my plan is to use it to write its own compiler.

## Requirements
- [A sane D compiler](https://dlang.org)
- [Any C compiler](https://www.bellard.org/tcc/) (optional, stdlib is bloat)

## Building
```sh
$ ./build_stdlib.sh # stdlib is optional
$ make
```

## Example
```c
// there are no builtin input/output functions,
// you have to write your own library if you want that functionality
// there's an example library in `lib/stdlib.c`. build with `./build_stdlib.sh`
extern "lib/stdlib.so" {
    print = fuck_print(_)
}

println("Hello, World!\n")
```
