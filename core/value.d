module fuck.core.value;
import std.string;
import std.array;
import core.sys.posix.dlfcn;
import core.stdc.stdlib;
import core.stdc.string;
import fuck.core.core;
import fuck.interpreter;
import fuck.expr;

extern(C):
struct FuckLib {
    string name;
    void *lib;
}

FuckLib[] fuck_libs;
void closeFuckLibs()
{
    while (fuck_libs.length) {
        dlclose(fuck_libs[$-1].lib);
        fuck_libs.popBack;
    }
}

void die(int num = 1)
{
    closeFuckLibs();
    exit(num);
}

enum Type {
    NUMBER,
    STRING,
    ARRAY,
    POINTER,
    FUNCTION,
    STRUCT,
}

struct String {
    ulong size;
    char *data;

    this(string s)
    {
        size = s.length;
        data = cast(char*) s.toStringz;
    }

    bool opEquals(String other)
    {
        if (size != other.size) return false;
        return (strncmp(data, other.data, size) == 0);
    }
}

struct ValueArray {
    ulong size;
    Value *data;

    this(Value[] a)
    {
        size = a.length;
        data = a.ptr;
    }
}

struct ValueFunction {
    // TODO: maybe also store their types
    string[] args;
    Expr expr;
}

struct Value {
    union Val {
        void *as_ptr;
        double as_num;
        String as_str;
        ValueArray as_arr;
        ValueFunction as_fun;
        Value[string] as_obj;
    }

    Type type;
    Val value;

    this(void *p)
    {
        type = Type.POINTER;
        value.as_ptr = p;
    }

    this(double n)
    {
        type = Type.NUMBER;
        value.as_num = n;
    }

    this(string s)
    {
        type = Type.STRING;
        value.as_str = String(s);
    }

    this (String s)
    {
        type = Type.STRING;
        value.as_str = s;
    }

    this(Value[] l)
    {
        type = Type.ARRAY;
        value.as_arr = ValueArray(l);
    }

    this(ValueFunction f)
    {
        type = Type.FUNCTION;
        value.as_fun = f;
    }

    this(Value[string] o)
    {
        type = Type.STRUCT;
        value.as_obj = o;
    }
}

bool equals(Value a, Value b)
{
    if (a.type != b.type)
        return false;

    switch (a.type) {
    case Type.NUMBER:
        return a.value.as_num == b.value.as_num;
    case Type.STRING:
        return a.value.as_str == b.value.as_str;
    case Type.ARRAY:
        return a.value.as_arr == b.value.as_arr;
    case Type.POINTER:
        return a.value.as_ptr == b.value.as_ptr;
    case Type.FUNCTION:
        return a.value.as_fun == b.value.as_fun;
    case Type.STRUCT:
        return a.value.as_obj == b.value.as_obj;
    default:
        return false;
    }
}

bool isTrue(Value a)
{
    if (a.type == Type.NUMBER && a.value.as_num == 0)
        return false;
    if (a.type == Type.STRING && a.value.as_str.size == 0)
        return false;
    if (a.type == Type.ARRAY && a.value.as_arr.size == 0)
        return false;
    return true;
}

extern(D) Value objGetField(Value[string] obj, string field)
{
    return obj[field];
}

extern(D) void objSetField(Value[string] obj, string field, Value value)
{
    if (field !in obj)
        obj["@fields"] = core_array_append([obj["@fields"], Value(field)]);
    obj[field] = value;
}

Value objArray(ulong size)
{
    Value[] l;
    foreach (i; 0..size)
        l ~= Value(0);
    return Value(l);
}

Value objGetField(Value[string] obj, char *field)
{
    return objGetField(obj, cast(string) field.fromStringz);
}

void objSetField(Value[string] obj, char *field, Value value)
{
    return objSetField(obj, cast(string) field.fromStringz, value);
}

extern(D) Value callFunction(ValueFunction fun, Value[] args)
{
    auto fuck = Interpreter([fun.expr]);
    return fuck.execFunction("<anon>", fun, args);
}

Value callFunction(ValueFunction fun, int argc, Value *argv)
{
    Value[] args;
    foreach (i; 0..argc)
        args ~= argv[i];
    return callFunction(fun, args);
}

// stupid dumb fuckery
string escape(string str)
{
    string s;
    int i = 0;
    while (i < str.length) {
        if (str[i] == '\\') {
            ++i;
            switch (str[i]) {
            case '\\': s ~= '\\'; break;
            case '\'': s ~= '\''; break;
            case '\"': s ~= '\"'; break;
            case 'n': s ~= '\n'; break;
            case 'r': s ~= '\r'; break;
            case 't': s ~= '\t'; break;
            case 'v': s ~= '\v'; break;
            case 'f': s ~= '\f'; break;
            case 'e': s ~= '\033'; break; // for terminal escape sequences
            default: s ~= str[i]; break;
            }
        }
        else {
            s ~= str[i];
        }
        ++i;
    }
    return s;
}
