module fuck.interpreter;
import std.stdio;
import std.array;
import std.string;
import std.conv;
import std.algorithm;
import std.file;
import core.stdc.stdlib;
import core.sys.posix.dlfcn;
import fuck.expr;
import fuck.core.value;
import fuck.core.binaryop;
import fuck.core.core;

Value function(ulong argc, Value *argv)[string] fuck_core;
Value function(Value *argv, int argc)[string] fuck_extern;
Value[] fuck_vargs;

static void loadFuckCore(string[] args)
{
    if (args.length > 1)
        args[1..$].each!(a => fuck_vargs ~= Value(a));

    fuck_core["arguments"] = (argc, argv) => Value(fuck_vargs);
    fuck_core["string"]    = &core_string;
    fuck_core["len"]       = &core_array_len;
    fuck_core["append"]    = &core_array_append;
    fuck_core["insert"]    = &core_array_insert;
    fuck_core["remove"]    = &core_array_remove;
}

alias Function = ExprMakeFunction;
alias Structure = ExprMakeStruct;

struct Scope {
    string name;
    Value[string] vars;
    Function[string] funs;
    Structure[string] objs;

    this(string n)
    {
        name = n;
    }
}

struct Interpreter {
    Expr[] ast;
    Scope *global = new Scope("<global>");
    Scope[] scopes;
    bool is_return;
    bool is_break;
    bool is_next;

    Value run(Expr[] block)
    {
        auto blocks = ["if", "while"];
        Value res;
        for (int i = 0; i < block.length; ++i) {
            res = doExpr(block[i]);
            
            if (is_return || is_break) {
                break;
            }
            else if (is_next) {
                ++i;
                is_next = false;
            }
        }

        if (!blocks.canFind(currentScope.name))
            is_return = false;
        /* is_break = false; */
        /* is_next = false; */
        return res;
    }

private:
    Scope* currentScope()
    {
        if (scopes.length == 0)
            return global;
        return &scopes[$-1];
    }

    Scope* lastScope()
    {
        if (scopes.length < 2)
            return global;
        return &scopes[$-2];
    }

    Function getFunction(string name)
    {
        // TODO: check core functions + stdlib
        if (name in global.funs)
            return global.funs[name];
        else if (name in currentScope.funs)
            return currentScope.funs[name];

        stderr.writefln("error: function '%s' does not exist", name);
        die();
        assert(0); // make compiler happy
    }

    Value getVariable(string name)
    {
        if (name in global.vars)
            return global.vars[name];
        else if (name in currentScope.vars)
            return currentScope.vars[name];

        stderr.writefln("error: variable '%s' does not exist", name);
        die();
        assert(0); // make compiler happy
    }

    Value execFunction(string name, Expr func, Value[] args)
    {
        auto fn = getFunction(name);
        scopes ~= Scope(name);

        if (fn.args.length != args.length) {
            stderr.writefln("error: function %s expects %d argument(s), got %d",
                    name, fn.args.length, args.length);
            die();
        }
        for (int i = 0; i < fn.args.length; ++i) {
            currentScope.vars[fn.args[i]] = args[i];
        }

        auto res = doExpr(func);
        scopes.popBack;
        return res;
    }

    Value createStruct(string name, Value[] args)
    {
        auto struc = (name in currentScope.objs)?
                        currentScope.objs[name] : global.objs[name];

        if (args.length != struc.fields.length) {
            stderr.writefln("error: struct %s expects %d field(s), got %d",
                    name, struc.fields.length, args.length);
            die();
        }

        Value[string] res;
        res["@typeof"] = Value(name);

        foreach (i; 0..args.length) {
            res[struc.fields[i]] = args[i];
        }

        return Value(res);
    }

    Value doExpr(Expr expr)
    {
        switch (expr.type) {
        case ExprType.LITERAL:
            return doLiteral(cast(ExprLiteral) expr);
        case ExprType.EXTERN:
            return doExtern(cast(ExprExtern) expr);
        case ExprType.SET_VARIABLE:
            return doSetVariable(cast(ExprSetVariable) expr);
        case ExprType.GET_VARIABLE:
            return doGetVariable(cast(ExprGetVariable) expr);
        case ExprType.MAKE_FUNCTION:
            return doMakeFunction(cast(ExprMakeFunction) expr);
        case ExprType.CALL_FUNCTION:
            return doCallFunction(cast(ExprCallFunction) expr);
        case ExprType.MAKE_STRUCT:
            return doMakeStruct(cast(ExprMakeStruct) expr);
        case ExprType.GET_STRUCT_FIELD:
            return doGetStructField(cast(ExprGetStructField) expr);
        case ExprType.SET_STRUCT_FIELD:
            return doSetStructField(cast(ExprSetStructField) expr);
        case ExprType.MAKE_ARRAY:
            return doMakeArray(cast(ExprMakeArray) expr);
        case ExprType.GET_ARRAY_INDEX:
            return doGetArrayAtIndex(cast(ExprGetArrayAtIndex) expr);
        case ExprType.SET_ARRAY_INDEX:
            return doSetArrayAtIndex(cast(ExprSetArrayAtIndex) expr);
        case ExprType.IF:
            return doIf(cast(ExprIf) expr);
        case ExprType.WHILE:
            return doWhile(cast(ExprWhile) expr);
        case ExprType.RETURN:
            return doReturn(cast(ExprReturn) expr);
        case ExprType.BREAK:
            return doBreak(cast(ExprBreak) expr);
        case ExprType.BLOCK:
            return doBlock(cast(ExprBlock) expr);
        case ExprType.UNARY_OP:
            return doUnaryOp(cast(ExprUnaryOp) expr);
        case ExprType.BINARY_OP:
            return doBinaryOp(cast(ExprBinaryOp) expr);
        default:
            stderr.writefln("error: unexpected %s", to!string(expr.type));
            die();
        }
        assert(0); // make the compiler happy
    }

    Value doLiteral(ExprLiteral expr)
    {
        return expr.value;
    }

    Value doExtern(ExprExtern expr)
    {
        auto lib = expr.lib;
        auto funs = expr.funs;

        if (!lib.exists || lib.isDir) {
            stderr.writefln("error: could not load library '%s'", lib);
            die();
        }

        void *lh = dlopen(lib.toStringz, RTLD_LAZY);
        fuck_libs ~= lh;
        if (!lh) {
            stderr.writefln("error: could not load library '%s'", lib);
            die();
        }

        foreach (fun; funs) {
            fuck_extern[fun.name] = cast(Value function(Value*, int))
                                            dlsym(lh, fun.func.toStringz);
            char *error = dlerror();
            if (error != null) {
                stderr.writefln("error: %s: %s", lib, error.fromStringz);
                die();
            }
        }

        return Value(0);
    }

    Value doSetVariable(ExprSetVariable expr)
    {
        Value value = doExpr(expr.value);
        currentScope.vars[expr.name] = value;
        return value;
    }

    Value doGetVariable(ExprGetVariable expr)
    {
        return getVariable(expr.name);
    }

    Value doMakeFunction(ExprMakeFunction expr)
    {
        auto sc = currentScope;
        auto name = String(expr.name);
        sc.funs[expr.name] = expr;
        return Value(0);
    }

    Value doCallFunction(ExprCallFunction expr)
    {
        Value[] args;
        Function func;
        auto name = expr.name;
        foreach (arg; expr.args)
            args ~= doExpr(arg);

        if (name in currentScope.funs) {
            func = currentScope.funs[name];
        }
        else if (name in global.funs) {
            func = global.funs[name];
        }
        else if (name in currentScope.objs || name in global.objs) {
            return createStruct(name, args);
        }
        else if (name in fuck_extern) {
            auto argc = to!int(args.length);
            auto argv = args.ptr;
            return fuck_extern[name](argv, argc);
        }
        else if (name in fuck_core) {
            return fuck_core[name](args.length, args.ptr);
        }
        else {
            stderr.writefln("error: function '%s' does not exist", name);
            die();
        }

        return execFunction(name, func.expr, args);
    }

    Value doMakeStruct(ExprMakeStruct expr)
    {
        Value[string] s;
        s["@typeof"] = Value(expr.name);

        foreach (field; expr.fields)
            s[field] = Value(0);

        currentScope.objs[expr.name] = expr;
        return Value(s);
    }

    Value doGetStructField(ExprGetStructField expr)
    {
        Value obj = doExpr(expr.struc);

        if (obj.type != Type.STRUCT) {
            stderr.writefln("error: struct.field expects (struct, word)");
            die();
        }

        if (expr.field !in obj.value.as_obj) {
            stderr.writefln("error: '%s' does not contain field '%s'",
                    core_string(obj.value.as_obj["@typeof"]), expr.field);
            die();
        }

        return obj.value.as_obj[expr.field];
    }

    Value doSetStructField(ExprSetStructField expr)
    {
        Value obj = doExpr(expr.struc);
        Value val = doExpr(expr.value);

        if (obj.type != Type.STRUCT) {
            stderr.writefln("error: struct.field expects (struct, word)");
            die();
        }

        if (expr.field !in obj.value.as_obj) {
            stderr.writefln("error: '%s' does not contain field '%s'",
                    core_string(obj.value.as_obj["@typeof"]), expr.field);
            die();
        }

        obj.value.as_obj[expr.field] = val;
        return val;
    }

    Value doMakeArray(ExprMakeArray expr)
    {
        Value[] res;
        foreach (e; expr.array)
            res ~= doExpr(e);
        return Value(res);
    }

    Value doGetArrayAtIndex(ExprGetArrayAtIndex expr)
    {
        Value array = doExpr(expr.array);
        Value index = doExpr(expr.index);

        if (array.type != Type.ARRAY || index.type != Type.NUMBER) {
            stderr.writefln("error: 'array[index]' expects (array, number)");
            die();
        }

        if (index.value.as_num < 0 || index.value.as_num >= array.value.as_arr.size) {
            stderr.writefln("error: array index out of range");
            die();
        }

        ulong idx = to!ulong(index.value.as_num);
        return array.value.as_arr.data[idx];
    }

    Value doSetArrayAtIndex(ExprSetArrayAtIndex expr)
    {
        Value array = doExpr(expr.array);
        Value index = doExpr(expr.index);
        Value value = doExpr(expr.value);

        if (array.type != Type.ARRAY || index.type != Type.NUMBER) {
            stderr.writefln("error: 'array[index]' expects (array, number)");
            die();
        }

        if (index.value.as_num < 0 || index.value.as_num >= array.value.as_arr.size) {
            stderr.writefln("error: array index out of range");
            die();
        }

        ulong idx = to!ulong(index.value.as_num);
        array.value.as_arr.data[idx] = value;
        return value;
    }

    Value doIf(ExprIf expr)
    {
        Value res;
        scopes ~= Scope("if");
        currentScope.vars = lastScope.vars;
        currentScope.funs = lastScope.funs;
        if (doExpr(expr.condition).isTrue) {
            res = doExpr(expr.expr);
        }
        else if (expr.has_else) {
            res = doExpr(expr.elze);
        }
        scopes.popBack;
        return res;
    }

    Value doWhile(ExprWhile expr)
    {
        Value res;
        scopes ~= Scope("while");
        currentScope.vars = lastScope.vars;
        currentScope.funs = lastScope.funs;
        while (doExpr(expr.condition).isTrue) {
            res = doExpr(expr.expr);
            if (expr.after)
                doExpr(expr.after);

            if (is_next) {
                is_next = false;
                continue;
            }
            if (is_return || is_break) {
                is_break = false;
                break;
            }
        }
        scopes.popBack;
        return res;
    }

    Value doBlock(ExprBlock expr)
    {
        return run(expr.block);
    }

    Value doReturn(ExprReturn expr)
    {
        Value res = doExpr(expr.expr);
        is_return = true;
        return res;
    }

    Value doBreak(ExprBreak expr)
    {
        if (expr.is_next) is_next = true;
        else is_break = true;
        return Value(0);
    }

    Value doUnaryOp(ExprUnaryOp expr)
    {
        Value res = doExpr(expr.value);
        switch (expr.op) {
        case "-":
            return opMath(expr.op, Value(0), res);
        case "!":
            return Value(to!double(!res.isTrue));
        default:
            stderr.writefln("error: unexpected operator '%s'", expr.op);
            die();
        }
        assert(0); // make compiler 
    }

    Value doBinaryOp(ExprBinaryOp expr)
    {
        auto maths = ["+", "-", "*", "/"];
        auto comparison = ["==", "!=", ">", "<", ">=", "<=", "&&", "||"];
        auto a = doExpr(expr.left);
        auto b = doExpr(expr.right);
        if (maths.canFind(expr.op))
            return opMath(expr.op, a, b);
        if (comparison.canFind(expr.op))
            return opCompare(expr.op, a, b);

        stderr.writefln("error: unexpected operator '%s'", expr.op);
        die();
        assert(0); // make happy
    }
}

