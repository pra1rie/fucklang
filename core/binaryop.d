module fuck.core.binaryop;
import fuck.core.core;
import fuck.core.value;
import std.stdio;
import std.conv;
import core.stdc.stdlib;

static void expectNumbers(string op, Value a, Value b)
{
    if (a.type != Type.NUMBER || b.type != Type.NUMBER) {
        stderr.writefln("error: '%s' expected numbers, got '%s' and '%s'",
                op, core_string(a), core_string(b));
        die();
    }
}

Value opMath(string op, Value a, Value b)
{
    expectNumbers(op, a, b);
    switch (op) {
    case "+":
        return Value(a.value.as_num + b.value.as_num);
    case "-":
        return Value(a.value.as_num - b.value.as_num);
    case "*":
        return Value(a.value.as_num * b.value.as_num);
    case "/":
        if (b.value.as_num == 0) {
            stderr.writefln("error: division by zero");
            die();
        }
        return Value(a.value.as_num / b.value.as_num);
    default:
        stderr.writefln("unsupported operation '%s'", op);
        die();
    }
    assert(0); // make
}

Value opCompare(string op, Value a, Value b)
{
    switch (op) {
    case "&&":
        return Value(to!double(a.isTrue && b.isTrue));
    case "||":
        return Value(to!double(a.isTrue || b.isTrue));
    case "==":
        return Value(to!double(a.equals(b)));
    case "!=":
        return Value(to!double(!a.equals(b)));
    case ">":
        expectNumbers(op, a, b);
        return Value(to!double(a.value.as_num > b.value.as_num));
    case "<":
        expectNumbers(op, a, b);
        return Value(to!double(a.value.as_num < b.value.as_num));
    case ">=":
        expectNumbers(op, a, b);
        return Value(to!double(a.value.as_num >= b.value.as_num));
    case "<=":
        expectNumbers(op, a, b);
        return Value(to!double(a.value.as_num <= b.value.as_num));
    default:
        stderr.writefln("unsupported operation '%s'", op);
        die();
    }
    assert(0); // happy
}
