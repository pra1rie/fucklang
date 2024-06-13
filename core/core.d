module fuck.core.core;
import std.stdio;
import std.string;
import std.conv;
import core.stdc.stdlib;
import fuck.core.value;


static void expect(ulong argc, ulong nargs, string name)
{
    string sargs = (nargs == 1)? "argument" : "arguments";

    if (argc < nargs) {
        stderr.writefln("error: '%s' expects %d %s, got %d",
                name, nargs, sargs, argc);
        die();
    }
}

extern(C) immutable(char*) core_cstring(Value arg)
{
    return core_string(arg).toStringz;
}

string core_string(Value val, bool escape = false)
{
    switch (val.type) {
    case Type.NUMBER:
        return to!string(val.value.as_num);
    case Type.STRING:
        auto res = val.value.as_str.data.fromStringz;
        return cast(string) ((escape)? "\"" ~ res ~ "\"" : res);
    case Type.ARRAY:
        auto arr = "[";
        foreach (j; 0..val.value.as_arr.size) {
            if (j > 0) arr ~= ", ";
            arr ~= core_string(val.value.as_arr.data[j], true);
        }
        return arr ~ "]";
    default:
        return "nil";
    }
}

Value core_string(Value[] args)
{
    args.length.expect(1, "string");
    string str;
    foreach (val; args)
        str ~= core_string(val);
    return Value(str);
}

Value core_array_len(Value[] args)
{
    args.length.expect(1, "len");
    auto arr = args[0];
    if (arr.type != Type.ARRAY) {
        stderr.writefln("error: 'len' expected array, got '%s'", core_string(arr));
        die();
    }

    return Value(to!double(arr.value.as_arr.size));
}

Value core_array_append(Value[] args)
{
    args.length.expect(2, "append");
    auto arr = args[0];
    if (arr.type != Type.ARRAY) {
        stderr.writefln("error: 'append' expected array, got '%s'", core_string(arr));
        die();
    }

    Value val = args[1];
    Value[] ar;
    foreach(i; 0..arr.value.as_arr.size)
        ar ~= arr.value.as_arr.data[i];
    ar ~= val;
    arr = Value(ar);
    return arr;
}

Value core_array_insert(Value[] args)
{
    args.length.expect(3, "insert");
    auto arr = args[0];
    if (arr.type != Type.ARRAY) {
        stderr.writefln("error: 'insert' expected array, got '%s'", core_string(arr));
        die();
    }
    if (args[1].type != Type.NUMBER) {
        stderr.writefln("error: 'insert' expected number, got '%s'", core_string(args[1]));
        die();
    }

    Value val = args[2];
    ulong idx = to!ulong(args[1].value.as_num);
    if (idx > arr.value.as_arr.size) {
        stderr.writefln("error: array index out of range");
        die();
    }

    Value[] ar;
    foreach (i; 0..arr.value.as_arr.size) {
        if (i == idx)
            ar ~= val;
        ar ~= arr.value.as_arr.data[i];
    }

    arr = Value(ar);
    return arr;
}

Value core_array_remove(Value[] args)
{
    args.length.expect(2, "remove");
    auto arr = args[0];
    if (arr.type != Type.ARRAY) {
        stderr.writefln("error: 'remove' expected array, got '%s'", core_string(arr));
        die();
    }
    if (args[1].type != Type.NUMBER) {
        stderr.writefln("error: 'remove' expected number, got '%s'", core_string(args[1]));
        die();
    }

    ulong idx = to!ulong(args[1].value.as_num);
    if (idx > arr.value.as_arr.size) {
        stderr.writefln("error: array index out of range");
        die();
    }

    Value[] ar;
    foreach (i; 0..arr.value.as_arr.size) {
        if (i == idx) continue;
        ar ~= arr.value.as_arr.data[i];
    }

    arr = Value(ar);
    return arr;
}
