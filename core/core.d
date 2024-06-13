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

string core_string(Value arg, bool escape = false)
{
    auto res = to!string(core_string(1, &arg).value.as_str.data);
    if (escape && arg.type == Type.STRING) {
        return "\"" ~ res ~ "\"";
    }
    return res;
}

Value core_string(ulong argc, Value *argv)
{
    argc.expect(1, "string");
    string str;

    foreach (i; 0..argc) {
        auto val = argv[i];

        switch (val.type) {
        case Type.NUMBER:
            str ~= to!string(val.value.as_num);
            break;
        case Type.STRING:
            str ~= val.value.as_str.data.fromStringz;
            break;
        case Type.ARRAY:
            auto arr = "[";
            foreach (j; 0..val.value.as_arr.size) {
                if (j > 0) arr ~= ", ";
                arr ~= core_string(val.value.as_arr.data[j], true);
            }
            str ~= arr ~ "]";
            break;
        default:
            str ~= "nil";
            break;
        }
    }
    return Value(str);
}

Value core_array_len(ulong argc, Value *argv)
{
    argc.expect(1, "len");
    Value arr = argv[0];
    if (arr.type != Type.ARRAY) {
        stderr.writefln("error: 'len' expected array, got '%s'", core_string(arr));
        die();
    }

    return Value(to!double(arr.value.as_arr.size));
}

Value core_array_append(ulong argc, Value *argv)
{
    argc.expect(2, "append");
    Value arr = argv[0];
    if (arr.type != Type.ARRAY) {
        stderr.writefln("error: 'append' expected array, got '%s'", core_string(arr));
        die();
    }

    Value val = argv[1];
    Value[] ar;
    foreach(i; 0..arr.value.as_arr.size)
        ar ~= arr.value.as_arr.data[i];
    ar ~= val;
    arr = Value(ar);
    return arr;
}

Value core_array_insert(ulong argc, Value *argv)
{
    argc.expect(3, "insert");
    Value arr = argv[0];
    if (arr.type != Type.ARRAY) {
        stderr.writefln("error: 'insert' expected array, got '%s'", core_string(arr));
        die();
    }
    if (argv[1].type != Type.NUMBER) {
        stderr.writefln("error: 'insert' expected number, got '%s'", core_string(argv[1]));
        die();
    }

    Value val = argv[2];
    ulong idx = to!ulong(argv[1].value.as_num);
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

Value core_array_remove(ulong argc, Value *argv)
{
    argc.expect(2, "remove");
    Value arr = argv[0];
    if (arr.type != Type.ARRAY) {
        stderr.writefln("error: 'remove' expected array, got '%s'", core_string(arr));
        die();
    }
    if (argv[1].type != Type.NUMBER) {
        stderr.writefln("error: 'remove' expected number, got '%s'", core_string(argv[1]));
        die();
    }

    ulong idx = to!ulong(argv[1].value.as_num);
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
