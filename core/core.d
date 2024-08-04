module fuck.core.core;
import std.stdio;
import std.string;
import std.algorithm;
import std.array;
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

extern(C) immutable(char*) core_ctypeof(Value arg)
{
    return core_typeof(arg).toStringz;
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

string core_typeof(Value val)
{
    auto types = ["Number", "String", "Array", "Pointer", "Function"];
    switch (val.type) {
    case Type.STRUCT:
        return core_string(val.value.as_obj["@typeof"]);
    default:
        assert(val.type < types.length);
        return types[val.type];
    }
}

Value core_typeof(Value[] args)
{
    args.length.expect(1, "typeof");
    return Value(core_typeof(args[0]));
}

Value core_copy(Value val)
{
    switch (val.type) {
    case Type.STRUCT:
        return Value(val.value.as_obj.dup());
    case Type.ARRAY:
        return Value((*cast(Value[]*)(&val.value.as_arr)).dup());
    default:
        return val;
    }
}

Value core_copy(Value[] args)
{
    args.length.expect(1, "copy");
    return core_copy(args[0]);
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
    case Type.FUNCTION:
        auto str = "def (";
        foreach (i, arg; val.value.as_fun.args) {
            if (i > 0) str ~= ", ";
            str ~= arg;
        }
        return str ~ ")";
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

Value core_string_append(Value str, Value val)
{
    return Value(core_string(str) ~ core_string(val));
}

Value core_string_insert(Value str, Value idx, Value val)
{
    string res = core_string(str);
    auto i = to!ulong(idx.value.as_num);

    if (i >= res.length) {
        stderr.writefln("error: string index out of range");
        die();
    }

    res = res[0..i] ~ core_string(val) ~ res[i..$];
    return Value(res);
}

Value core_string_remove(Value str, Value idx)
{
    string res = core_string(str);
    auto i = to!ulong(idx.value.as_num);
    if (i >= res.length) {
        stderr.writefln("error: string index out of range");
        die();
    }

    if (i == res.length-1) res = res[0..$-1];
    else res = res[0..i] ~ res[i+1..$];
    return Value(res);
}

Value core_alloc(Value[] args)
{
    args.length.expect(1, "alloc");
    if (args[0].type != Type.NUMBER) {
        stderr.writefln("error: 'alloc' expects (Number), got (%s)",
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
        stderr.writefln("error: 'realloc' expects (Pointer, Number), got (%s, %s)",
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
        stderr.writefln("error: 'free' expects (Pointer), got (%s)",
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
        stderr.writefln("error: 'len' expects (Array), got (%s)",
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
    auto val = args[1];

    if (arr.type == Type.STRING && args[1].type == Type.STRING)
        return core_string_append(arr, args[1]);
    if (arr.type != Type.ARRAY) {
        stderr.writefln("error: 'append' expects (Array, Any), got (%s, %s)",
                core_string(arr, true), core_string(args[1], true));
        die();
    }

    auto ar = *cast(Value[]*)&arr.value.as_arr;
    ar ~= val;
    return Value(ar);
}

Value core_array_insert(Value[] args)
{
    args.length.expect(3, "insert");
    auto arr = args[0];
    auto idx = args[1];
    auto val = args[2];

    if (arr.type == Type.STRING && idx.type == Type.NUMBER && val.type == Type.STRING)
        return core_string_insert(arr, idx, val);
    if (arr.type != Type.ARRAY || idx.type != Type.NUMBER) {
        stderr.writefln("error: 'insert' expects (Array, Number, Any), got (%s, %s, %s)",
                core_string(arr, true), core_string(idx, true), core_string(val, true));
        die();
    }

    ulong index = to!ulong(idx.value.as_num);
    if (index > arr.value.as_arr.size) {
        stderr.writefln("error: array index out of range");
        die();
    }

    auto ar = *cast(Value[]*)&arr.value.as_arr;
    ar.insertInPlace(index, val);
    return Value(ar);
}

Value core_array_remove(Value[] args)
{
    args.length.expect(2, "remove");
    auto arr = args[0];
    auto idx = args[1];
    if (arr.type == Type.STRING && idx.type == Type.NUMBER)
        return core_string_remove(arr, idx);
    if (arr.type != Type.ARRAY || idx.type != Type.NUMBER) {
        stderr.writefln("error: 'remove' expects (Array, Number), got (%s, %s)",
                core_string(arr, true), core_string(idx, true));
        die();
    }

    ulong index = to!ulong(args[1].value.as_num);
    if (index >= arr.value.as_arr.size) {
        stderr.writefln("error: array index out of range");
        die();
    }

    auto ar = *cast(Value[]*)&arr.value.as_arr;
    ar = (ar).remove(index);
    return Value(ar);
}
