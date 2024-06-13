module fuck.core.core;
import std.stdio;
import std.string;
import std.conv;
import core.stdc.stdlib;
import core.memory;
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

extern(C) void* core_alloc(ulong sz)
{
    return core_alloc([Value(to!double(sz))]).value.as_ptr;
}

extern(C) void* core_realloc(void *ptr, ulong sz)
{
    return core_realloc([Value(ptr), Value(to!double(sz))]).value.as_ptr;
}

extern(C) void core_free(void *ptr)
{
    core_free([Value(ptr)]);
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
    case Type.STRUCT:
        auto obj = val.value.as_obj;
        auto str = core_string(obj["@typeof"]) ~ " {";
        auto fields = obj["@fields"].value.as_arr;
        foreach (i; 0..fields.size) {
            auto field = fields.data[i];
            if (i > 0) str ~= ", ";
            auto name = core_string(field);
            str ~= name ~ ": " ~ core_string(obj[name], true);
        }
        return str ~ "}";
    default:
        return "<ptr>";
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

Value core_alloc(Value[] args)
{
    args.length.expect(1, "alloc");
    if (args[0].type != Type.NUMBER) {
        stderr.writefln("error: 'alloc' expects (number), got (%s)",
                core_string(args[0], true));
        die();
    }

    auto sz = to!ulong(args[0].value.as_num);
    auto ptr = GC.calloc(sz);
    if (!ptr) {
        stderr.writefln("error: could not allocate memory of size %d", sz);
        die();
    }

    return Value(ptr);
}

Value core_realloc(Value[] args)
{
    args.length.expect(2, "realloc");
    if (args[0].type != Type.POINTER || args[1].type != Type.NUMBER) {
        stderr.writefln("error: 'realloc' expects (pointer, number), got (%s, %s)",
                core_string(args[0], true), core_string(args[1], true));
        die();
    }

    auto ptr = args[0].value.as_ptr;
    auto sz = to!ulong(args[1].value.as_num);

    ptr = GC.realloc(ptr, sz);
    return Value(ptr);
}

Value core_free(Value[] args)
{
    args.length.expect(1, "free");
    if (args[0].type != Type.POINTER) {
        stderr.writefln("error: 'free' expects (pointer), got (%s)",
                core_string(args[0], true));
        die();
    }

    auto ptr = args[0].value.as_ptr;
    GC.free(ptr);
    return Value(0);
}

Value core_array_len(Value[] args)
{
    args.length.expect(1, "len");
    auto arr = args[0];
    if (arr.type != Type.ARRAY && arr.type != Type.STRING) {
        stderr.writefln("error: 'len' expects (array), got (%s)",
                core_string(arr, true));
        die();
    }

    if (arr.type == Type.STRING)
        return Value(to!double(arr.value.as_str.size));
    return Value(to!double(arr.value.as_arr.size));
}

Value core_array_append(Value[] args)
{
    args.length.expect(2, "append");
    auto arr = args[0];
    if (arr.type != Type.ARRAY) {
        stderr.writefln("error: 'append' expects (array, any), got (%s, %s)",
                core_string(arr, true), core_string(args[1], true));
        die();
    }

    Value val = args[1];
    Value[] ar;
    foreach(i; 0..arr.value.as_arr.size)
        ar ~= arr.value.as_arr.data[i];
    ar ~= val;
    return Value(ar);
}

Value core_array_insert(Value[] args)
{
    args.length.expect(3, "insert");
    auto arr = args[0];
    auto idx = args[1];
    auto val = args[2];

    if (arr.type != Type.ARRAY || idx.type != Type.NUMBER) {
        stderr.writefln("error: 'insert' expects (array, number, any), got (%s, %s, %s)",
                core_string(arr, true), core_string(idx, true), core_string(val, true));
        die();
    }

    ulong index = to!ulong(idx.value.as_num);
    if (index > arr.value.as_arr.size) {
        stderr.writefln("error: array index out of range");
        die();
    }

    Value[] ar;
    foreach (i; 0..arr.value.as_arr.size) {
        if (i == index) ar ~= val;
        ar ~= arr.value.as_arr.data[i];
    }

    return Value(ar);
}

Value core_array_remove(Value[] args)
{
    args.length.expect(2, "remove");
    auto arr = args[0];
    auto idx = args[1];
    if (arr.type != Type.ARRAY || idx.type != Type.NUMBER) {
        stderr.writefln("error: 'remove' expects (array, number), got (%s, %s)",
                core_string(arr, true), core_string(idx, true));
        die();
    }

    ulong index = to!ulong(args[1].value.as_num);
    if (index > arr.value.as_arr.size) {
        stderr.writefln("error: array index out of range");
        die();
    }

    Value[] ar;
    foreach (i; 0..arr.value.as_arr.size) {
        if (i == index) continue;
        ar ~= arr.value.as_arr.data[i];
    }

    return Value(ar);
}
