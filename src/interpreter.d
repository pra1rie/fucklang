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
import fuck.parser;
import fuck.core.value;
import fuck.core.binaryop;
import fuck.core.core;

struct ExternFunction {
    /* string ret_type; */
    string[] types;
    Value function(Value *argv, int argc) func;
}

Value function(Value[] args)[string] fuck_core;
ExternFunction[string] fuck_extern;
Value[] fuck_vargs;

static void loadFuckCore(string[] args)
{
    if (args.length > 1)
        args[1..$].each!(a => fuck_vargs ~= Value(a));

    fuck_core["arguments"] = (args) => Value(fuck_vargs);
    fuck_core["typeof"]    = &core_typeof;
    fuck_core["copy"]      = &core_copy;
    fuck_core["string"]    = &core_string;
    fuck_core["alloc"]     = &core_alloc;
    fuck_core["realloc"]   = &core_realloc;
    fuck_core["free"]      = &core_free;
    fuck_core["len"]       = &core_array_len;
    fuck_core["append"]    = &core_array_append;
    fuck_core["insert"]    = &core_array_insert;
    fuck_core["remove"]    = &core_array_remove;
}

struct Structure {
    string name;
    Value[string] fields;
}

struct Scope {
    string name;
    Value[string] vars;
    Structure[string] objs;

    this(string n)
    {
        name = n;
    }
}

auto global = Scope("<global>");

struct Interpreter {
    Expr[] ast;
    Scope[] scopes;
    bool is_return;
    bool is_break;
    bool is_next;

    Value run(Expr[] block)
    {
        auto blocks = ["if", "while", "case"];
        Value res;
        for (int i = 0; i < block.length; ++i) {
            res = doExpr(block[i]);

            if (is_return || is_break) break;
            if (is_next) {
                ++i;
                is_next = false;
            }
        }

        if (!blocks.canFind(currentScope.name))
            is_return = false;
        return res;
    }

    Scope* currentScope()
    {
        if (scopes.length == 0)
            return &global;
        return &scopes[$-1];
    }

    Scope* lastScope()
    {
        if (scopes.length < 2)
            return &global;
        return &scopes[$-2];
    }

    ValueFunction getFunction(Loc loc, string name)
    {
        auto var = getVariable(loc, name);
        if (var.type != Type.FUNCTION) {
            stderr.writefln("%s: error: '%s' is not a function", loc.get, name);
            die();
        }

        return var.value.as_fun;
    }

    Value getVariable(Loc loc, string name)
    {
        if (name in currentScope.vars)
            return currentScope.vars[name];
        else if (name in global.vars)
            return global.vars[name];

        stderr.writefln("%s: error: variable '%s' does not exist", loc.get, name);
        die();
        assert(0); // make compiler happy
    }

    Value execFunction(string name, ValueFunction func, Value[] args)
    {
        scopes ~= Scope(name);
        auto al = args.length, fl = func.args.length;
        fl = (func.varg)? fl-1 : fl;
        if (al < fl || (!func.varg && fl > al)) {
            auto nargs = (func.args.length == 1)? "argument" : "arguments";
            stderr.writefln("%s: error: function '%s' expects %d %s, got %d",
                    func.expr.loc.get, name, fl, nargs, al);
            die();
        }

        currentScope.vars = lastScope.vars.dup;
        for (int i = 0; i < fl; ++i)
            currentScope.vars[func.args[i]] = args[i];
        currentScope.vars["va_list"] = Value(args);

        auto res = doExpr(func.expr);
        scopes.popBack;
        return res;
    }

    Value createStruct(Loc loc, string name, Value[] args)
    {
        auto struc = (name in currentScope.objs)?
                        currentScope.objs[name] : global.objs[name];

        auto fields = struc.fields["@fields"].value.as_arr;
        if (args.length > fields.size) {
            stderr.writefln("%s: error: struct %s expects %d field(s), got %d",
                    loc.get, name, fields.size, args.length);
            die();
        }

        Value[string] res;
        res["@typeof"] = Value(name);
        res["@fields"] = struc.fields["@fields"];

        foreach (i; 0..fields.size) {
            auto field = cast(string)fields.data[i].value.as_str.data.fromStringz;
            res[field] = (i < args.length)? args[i] : struc.fields[field];
        }

        return Value(res);
    }

private:
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
        case ExprType.MAKE_ENUM:
            return doEnum(cast(ExprMakeEnum) expr);
        case ExprType.CASE:
            return doCase(cast(ExprCase) expr);
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
            stderr.writefln("%s: error: unexpected %s", expr.loc.get, to!string(expr.type));
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
        auto lib = expr.lib.getFileFromPath;
        auto funs = expr.funs;
        void *lh = null;


        // don't load lib twice
        foreach(l; fuck_libs) {
            if (l.name == lib) {
                lh = l.lib;
                break;
            }
        }
        if (!lh) {
            lh = dlopen(lib.toStringz, RTLD_LAZY);
            fuck_libs ~= FuckLib(lib, lh);
        }

        if (!lh) {
            stderr.writefln("%s: error: could not load library '%s'", expr.loc.get, lib);
            die();
        }

        foreach (fun; funs) {
            auto f = cast(Value function(Value*, int)) dlsym(lh, fun.func.toStringz);
            char *error = dlerror();
            if (error != null) {
                stderr.writefln("%s: error: %s: %s", expr.loc.get, lib, error.fromStringz);
                die();
            }
            fuck_extern[fun.name] = ExternFunction(fun.types, f);
        }

        return Value(0);
    }

    Value doSetVariable(ExprSetVariable expr)
    {
        Value value = doExpr(expr.value);
        // make a copy if assigning to struct
        value = (value.type == Type.STRUCT)? Value(value.value.as_obj.dup()) : value;
        // global variables aaaaaaaaaaaaaaaaaaaaa
        /* if (expr.name in global.vars) global.vars[expr.name] = value; */
        currentScope.vars[expr.name] = value;
        return value;
    }

    Value doGetVariable(ExprGetVariable expr)
    {
        return getVariable(expr.loc, expr.name);
    }

    Value doMakeFunction(ExprMakeFunction expr)
    {
        auto sc = currentScope;
        auto func = Value(ValueFunction(expr.args, expr.expr, expr.varg));
        if (expr.name)
            sc.vars[expr.name] = func;
        return func;
    }

    Value doCallFunction(ExprCallFunction expr)
    {
        Value[] args;
        foreach (arg; expr.args)
            args ~= doExpr(arg);

        if (expr.func.type != ExprType.GET_VARIABLE) {
            auto func = doExpr(expr.func);
            auto name = "<anon>";
            if (func.type != Type.FUNCTION) {
                stderr.writefln("%s: error: expected function, got '%s'",
                        expr.loc.get, core_string(func));
                die();
            }
            // add struct as first argument if called with 'struct.field()'
            if (expr.func.type == ExprType.GET_STRUCT_FIELD) {
                auto s = doExpr((cast(ExprGetStructField)expr.func).struc);
                args = s ~ args;
                foreach (k; s.value.as_obj.byKeyValue) {
                    if (k.value == func) {
                        name = k.key;
                        break;
                    }
                }
            }
            return execFunction(name, func.value.as_fun, args);
        }

        auto name = (cast(ExprGetVariable)expr.func).name;

        if (name in currentScope.vars || name in global.vars) {
            return execFunction(name, getFunction(expr.loc, name), args);
        }
        else if (name in currentScope.objs || name in global.objs) {
            return createStruct(expr.loc, name, args);
        }
        else if (name in fuck_extern) {
            auto fn = fuck_extern[name];
            auto argc = to!int(args.length);
            auto argv = args.ptr;
            // don't check number of arguments if last argument is a wildcard (_)
            if ((fn.types.length != 0 && fn.types[$-1] != "_")) {
                if (argc != fn.types.length) {
                    auto nargs = fn.types.length == 1? "argument" : "arguments";
                    stderr.writefln("%s: error: function '%s' expects %d %s, got %d",
                            expr.loc.get, name, fn.types.length, nargs, argc);
                    die();
                }
            }

            // typecheck arguments unless type is wildcard (_)
            foreach (i; 0..argc) {
                if (i >= fn.types.length) break; // this is fine since number of arguments was already checked
                if (fn.types[i] != "_" && core_typeof(args[i]) != fn.types[i]) {
                    stderr.writefln("%s: error: function '%s' expects '%s', got '%s'",
                            expr.loc.get, name, fn.types[i], core_string(args[i]));
                    die();
                }
            }
            return fn.func(argv, argc);
        }
        else if (name in fuck_core) {
            return fuck_core[name](args);
        }
        stderr.writefln("%s: error: function '%s' does not exist", expr.loc.get, name);
        die();
        assert(0); // maek complier happy
    }

    Value doMakeStruct(ExprMakeStruct expr)
    {
        Value[string] s;
        auto field_names = doExpr(expr.fields["@fields"]);

        s["@typeof"] = Value(expr.name);
        s["@fields"] = field_names;

        foreach (field; expr.fields.byKeyValue)
            s[field.key] = doExpr(field.value);

        currentScope.objs[expr.name] = Structure(expr.name, s);
        return Value(s);
    }

    Value doGetStructField(ExprGetStructField expr)
    {
        Value obj = doExpr(expr.struc);

        if (obj.type != Type.STRUCT) {
            stderr.writefln("%s: error: struct.field expects (struct, word)", expr.loc.get);
            die();
        }

        if (expr.field !in obj.value.as_obj) {
            stderr.writefln("%s: error: '%s' does not contain field '%s'",
                    expr.loc.get, core_string(obj.value.as_obj["@typeof"]), expr.field);
            die();
        }

        return obj.value.as_obj[expr.field];
    }

    Value doSetStructField(ExprSetStructField expr)
    {
        Value obj = doExpr(expr.struc);
        Value val = doExpr(expr.value);

        if (obj.type != Type.STRUCT) {
            stderr.writefln("%s: error: struct.field expects (struct, word)", expr.loc.get);
            die();
        }

        if (expr.field !in obj.value.as_obj) {
            stderr.writefln("%s: error: '%s' does not contain field '%s'",
                    expr.loc.get, core_string(obj.value.as_obj["@typeof"]), expr.field);
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

        if (array.type == Type.STRING && index.type == Type.NUMBER)
            return doGetStringAtIndex(expr.loc, array, index);

        if (array.type != Type.ARRAY || index.type != Type.NUMBER) {
            stderr.writefln("%s: error: 'array[index]' expects (array, number)", expr.loc.get);
            die();
        }

        if (index.value.as_num < 0 || index.value.as_num >= array.value.as_arr.size) {
            stderr.writefln("%s: error: array index out of range", expr.loc.get);
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
            stderr.writefln("%s: error: 'array[index]' expects (array, number)", expr.loc.get);
            die();
        }

        if (index.value.as_num < 0 || index.value.as_num >= array.value.as_arr.size) {
            stderr.writefln("%s: error: array index out of range", expr.loc.get);
            die();
        }

        ulong idx = to!ulong(index.value.as_num);
        array.value.as_arr.data[idx] = value;
        return value;
    }

    Value doGetStringAtIndex(Loc loc, Value str, Value idx)
    {
        auto s = core_string(str);
        auto i = to!ulong(idx.value.as_num);
        if (i >= s.length) {
            stderr.writefln("%s: error: string index out of range", loc.get);
            die();
        }
        return Value(s[i] ~ "");
    }

    Value doEnum(ExprMakeEnum expr)
    {
        Value res;
        foreach (var; expr.vars)
            res = doExpr(var);
        return res;
    }

    Value doCase(ExprCase expr)
    {
        Value res;
        Value check = doExpr(expr.check);
        bool found;
        scopes ~= Scope("case");
        currentScope.vars = lastScope.vars;
        foreach (match; expr.matches) {
            if (check.equals(doExpr(match.match))) {
                res = doExpr(match.expr);
                found = true;
                break;
            }
        }
        if (expr.has_defalt && !found)
            res = doExpr(expr.defalt);
        scopes.popBack;
        return res;
    }

    Value doIf(ExprIf expr)
    {
        Value res;
        scopes ~= Scope("if");
        currentScope.vars = lastScope.vars;
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
            stderr.writefln("%s: error: unexpected operator '%s'", expr.loc.get, expr.op);
            die();
        }
        assert(0); // make compiler 
    }

    // most of the good languages have this thing where if you have 2 or more conditions
    // with && and one of them is false, it doesn't check the other
    // and i use that all the time, so i expect fuck to behave the same.
    Value doBinaryOp(ExprBinaryOp expr)
    {
        auto maths = ["+", "-", "*", "/", "%"];
        auto comparison = ["==", "!=", ">", "<", ">=", "<=", "&&", "||"];
        auto a = doExpr(expr.left);
        if (!a.isTrue && expr.op == "&&")
            return Value(0);
        auto b = doExpr(expr.right);
        if (maths.canFind(expr.op))
            return opMath(expr.op, a, b);
        if (comparison.canFind(expr.op))
            return opCompare(expr.op, a, b);

        stderr.writefln("%s: error: unexpected operator '%s'", expr.loc.get, expr.op);
        die();
        assert(0); // make happy
    }
}

