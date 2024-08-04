module fuck.expr;
import std.format;
import fuck.core.value;

enum ExprType {
    LITERAL,
    EXTERN,
    SET_VARIABLE,
    GET_VARIABLE,
    MAKE_FUNCTION,
    CALL_FUNCTION,
    MAKE_STRUCT,
    GET_STRUCT_FIELD,
    SET_STRUCT_FIELD,
    MAKE_ARRAY,
    GET_ARRAY_INDEX,
    SET_ARRAY_INDEX,
    MAKE_ENUM,
    CASE,
    IF,
    WHILE,
    RETURN,
    BREAK,
    BLOCK,
    UNARY_OP,
    BINARY_OP,
}

struct Loc {
    string path;
    ulong line = 1;

    string get()
    {
        return format("%s:%d", path, line);
    }
}

struct ExternFuncAlias {
    string name;
    string func;
    string[] types;
}

struct Match {
    Expr match;
    Expr expr;
}

class Expr
{
    ExprType type;
    Loc loc;

    this(Loc l, ExprType t)
    {
        loc = l;
        type = t;
    }
}

class ExprLiteral : Expr
{
    Value value;

    this(Loc loc, Value v)
    {
        super(loc, ExprType.LITERAL);
        value = v;
    }
}

class ExprExtern : Expr
{
    string lib;
    ExternFuncAlias[] funs;

    this(Loc loc, string l, ExternFuncAlias[] f)
    {
        super(loc, ExprType.EXTERN);
        lib = l;
        funs = f;
    }
}

class ExprSetVariable : Expr
{
    string name;
    Expr value;

    this(Loc loc, string n, Expr v)
    {
        super(loc, ExprType.SET_VARIABLE);
        name = n;
        value = v;
    }
}

class ExprGetVariable : Expr
{
    string name;

    this(Loc loc, string n)
    {
        super(loc, ExprType.GET_VARIABLE);
        name = n;
    }
}

class ExprMakeFunction : Expr
{
    string name;
    string[] args;
    Expr expr;

    this(Loc loc, string n, string[] a, Expr e)
    {
        super(loc, ExprType.MAKE_FUNCTION);
        name = n;
        args = a;
        expr = e;
    }
}

class ExprCallFunction : Expr
{
    /* string name; */
    Expr func;
    Expr[] args;

    this(Loc loc, Expr f, Expr[] a)
    {
        super(loc, ExprType.CALL_FUNCTION);
        /* name = n; */
        func = f;
        args = a;
    }
}

class ExprMakeStruct : Expr
{
    string name;
    Expr[string] fields;

    this(Loc loc, string n, Expr[string] f)
    {
        super(loc, ExprType.MAKE_STRUCT);
        name = n;
        fields = f;
    }
}

class ExprGetStructField : Expr
{
    Expr struc;
    string field;

    this(Loc loc, Expr s, string f)
    {
        super(loc, ExprType.GET_STRUCT_FIELD);
        struc = s;
        field = f;
    }
}

class ExprSetStructField : Expr
{
    Expr struc;
    string field;
    Expr value;

    this(Loc loc, Expr s, string f, Expr v)
    {
        super(loc, ExprType.SET_STRUCT_FIELD);
        struc = s;
        field = f;
        value = v;
    }
}

class ExprMakeArray : Expr
{
    Expr[] array;

    this(Loc loc, Expr[] a)
    {
        super(loc, ExprType.MAKE_ARRAY);
        array = a;
    }
}

class ExprGetArrayAtIndex : Expr
{
    Expr array;
    Expr index;

    this(Loc loc, Expr a, Expr i)
    {
        super(loc, ExprType.GET_ARRAY_INDEX);
        array = a;
        index = i;
    }
}

class ExprSetArrayAtIndex : Expr
{
    Expr array;
    Expr index;
    Expr value;

    this(Loc loc, Expr a, Expr i, Expr v)
    {
        super(loc, ExprType.SET_ARRAY_INDEX);
        array = a;
        index = i;
        value = v;
    }
}

class ExprMakeEnum : Expr {
    Expr[] vars;

    this(Loc loc, Expr[] v)
    {
        super(loc, ExprType.MAKE_ENUM);
        vars = v;
    }
}

class ExprCase : Expr
{
    Expr check;
    Match[] matches;
    Expr defalt;
    bool has_defalt;

    this(Loc loc, Expr c, Match[] m, Expr d, bool h = false)
    {
        super(loc, ExprType.CASE);
        check = c;
        matches = m;
        defalt = d;
        has_defalt = h;
    }
}

class ExprIf : Expr
{
    Expr condition;
    Expr expr;
    Expr elze;
    bool has_else;

    this(Loc loc, Expr c, Expr e, Expr z, bool h = false)
    {
        super(loc, ExprType.IF);
        condition = c;
        expr = e;
        elze = z;
        has_else = h;
    }
}

class ExprWhile : Expr
{
    Expr condition;
    Expr expr;
    Expr after;

    this(Loc loc, Expr c, Expr e, Expr a)
    {
        super(loc, ExprType.WHILE);
        condition = c;
        expr = e;
        after = a;
    }
}

class ExprReturn : Expr
{
    Expr expr;

    this(Loc loc, Expr e)
    {
        super(loc, ExprType.RETURN);
        expr = e;
    }
}

class ExprBreak : Expr
{
    bool is_next;

    this(Loc loc, bool n)
    {
        super(loc, ExprType.BREAK);
        is_next = n;
    }
}

class ExprBlock : Expr
{
    Expr[] block;

    this(Loc loc, Expr[] b)
    {
        super(loc, ExprType.BLOCK);
        block = b;
    }
}

class ExprUnaryOp : Expr
{
    string op;
    Expr value;

    this(Loc loc, string o, Expr v)
    {
        super(loc, ExprType.UNARY_OP);
        op = o;
        value = v;
    }
}

class ExprBinaryOp : Expr
{
    string op;
    Expr left;
    Expr right;

    this(Loc loc, string o, Expr a, Expr b)
    {
        super(loc, ExprType.BINARY_OP);
        op = o;
        left = a;
        right = b;
    }
}
