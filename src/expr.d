module fuck.expr;
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
    IF,
    WHILE,
    RETURN,
    BREAK,
    BLOCK,
    UNARY_OP,
    BINARY_OP,
}

struct ExternFuncAlias {
    string name;
    string func;
}

class Expr
{
    ExprType type;

    this(ExprType t)
    {
        type = t;
    }
}

class ExprLiteral : Expr
{
    Value value;

    this(Value v)
    {
        super(ExprType.LITERAL);
        value = v;
    }
}

class ExprExtern : Expr
{
    string lib;
    ExternFuncAlias[] funs;

    this(string l, ExternFuncAlias[] f)
    {
        super(ExprType.EXTERN);
        lib = l;
        funs = f;
    }
}

class ExprSetVariable : Expr
{
    string name;
    Expr value;

    this(string n, Expr v)
    {
        super(ExprType.SET_VARIABLE);
        name = n;
        value = v;
    }
}

class ExprGetVariable : Expr
{
    string name;

    this(string n)
    {
        super(ExprType.GET_VARIABLE);
        name = n;
    }
}

class ExprMakeFunction : Expr
{
    string name;
    string[] args;
    Expr expr;

    this(string n, string[] a, Expr e)
    {
        super(ExprType.MAKE_FUNCTION);
        name = n;
        args = a;
        expr = e;
    }
}

class ExprCallFunction : Expr
{
    string name;
    Expr[] args;

    this(string n, Expr[] a)
    {
        super(ExprType.CALL_FUNCTION);
        name = n;
        args = a;
    }
}

class ExprMakeStruct : Expr
{
    string name;
    string[] fields;

    this(string n, string[] f)
    {
        super(ExprType.MAKE_STRUCT);
        name = n;
        fields = f;
    }
}

class ExprGetStructField : Expr
{
    Expr struc;
    string field;

    this(Expr s, string f)
    {
        super(ExprType.GET_STRUCT_FIELD);
        struc = s;
        field = f;
    }
}

class ExprSetStructField : Expr
{
    Expr struc;
    string field;
    Expr value;

    this(Expr s, string f, Expr v)
    {
        super(ExprType.SET_STRUCT_FIELD);
        struc = s;
        field = f;
        value = v;
    }
}

class ExprMakeArray : Expr
{
    Expr[] array;

    this(Expr[] a)
    {
        super(ExprType.MAKE_ARRAY);
        array = a;
    }
}

class ExprGetArrayAtIndex : Expr
{
    Expr array;
    Expr index;

    this(Expr a, Expr i)
    {
        super(ExprType.GET_ARRAY_INDEX);
        array = a;
        index = i;
    }
}

class ExprSetArrayAtIndex : Expr
{
    Expr array;
    Expr index;
    Expr value;

    this(Expr a, Expr i, Expr v)
    {
        super(ExprType.SET_ARRAY_INDEX);
        array = a;
        index = i;
        value = v;
    }
}

class ExprIf : Expr
{
    Expr condition;
    Expr expr;
    Expr elze;
    bool has_else;

    this(Expr c, Expr e, Expr z, bool h = false)
    {
        super(ExprType.IF);
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

    this(Expr c, Expr e, Expr a)
    {
        super(ExprType.WHILE);
        condition = c;
        expr = e;
        after = a;
    }
}

class ExprReturn : Expr
{
    Expr expr;

    this(Expr e)
    {
        super(ExprType.RETURN);
        expr = e;
    }
}

class ExprBreak : Expr
{
    bool is_next;

    this(bool n)
    {
        super(ExprType.BREAK);
        is_next = n;
    }
}

class ExprBlock : Expr
{
    Expr[] block;

    this(Expr[] b)
    {
        super(ExprType.BLOCK);
        block = b;
    }
}

class ExprUnaryOp : Expr
{
    string op;
    Expr value;

    this(string o, Expr v)
    {
        super(ExprType.UNARY_OP);
        op = o;
        value = v;
    }
}

class ExprBinaryOp : Expr
{
    string op;
    Expr left;
    Expr right;

    this(string o, Expr a, Expr b)
    {
        super(ExprType.BINARY_OP);
        op = o;
        left = a;
        right = b;
    }
}
