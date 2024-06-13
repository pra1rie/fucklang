module fuck.core.value;
import std.string;
import core.sys.posix.dlfcn;
import core.stdc.stdlib;
import core.stdc.string;


extern(C):
void*[] fuck_libs;
void closeFuckLibs()
{
    foreach (lib; fuck_libs)
        dlclose(lib);
    fuck_libs = [];
}

void die()
{
    closeFuckLibs();
    exit(1);
}

enum Type {
    NUMBER,
    STRING,
    ARRAY,
    POINTER,
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

struct Value {
    union Val {
        void *as_ptr;
        double as_num;
        String as_str;
        ValueArray as_arr;
        Value[string] as_obj;
    }

    Type type;
    Val value;

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
    obj[field] = value;
}

Value objGetField(Value[string] obj, char *field)
{
    return objGetField(obj, cast(string) field.fromStringz);
}

void objSetField(Value[string] obj, char *field, Value value)
{
    return objSetField(obj, cast(string) field.fromStringz, value);
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