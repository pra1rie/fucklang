module fuck.main;
import core.stdc.stdlib;
import std.stdio;
import std.conv;
import fuck.parser;
import fuck.interpreter;
import fuck.core.value;

// TODO: functions for joining arrays, splitting/replacing strings, ...
// TODO: allow for importing multiple files with a single 'import'
// TODO: look at $PATH for 'import' and 'extern'
// TODO: allow default values for functions
// TODO: call functions with dot syntax (foo.bar())
// TODO: better error reports (they still kinda suck, also
//          it doesn't show the correct line number at times)

void main(string[] args)
{
    if (args.length < 2) {
        stderr.writefln("usage: %s <file>", args[0]);
        exit(1);
    }
    auto path = args[1];
    auto ast = parseFile(path);

    loadFuckCore(args);
    auto fuck = Interpreter(ast);
    fuck.run(fuck.ast);
    closeFuckLibs();
}
