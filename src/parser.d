module fuck.parser;
import core.stdc.stdlib;
import std.algorithm;
import std.array;
import std.stdio;
import std.format;
import std.file;
import std.ascii;
import std.conv;
import std.typecons;
import fuck.expr;
import fuck.core.value;

immutable string[] keywords = [
    "def", "return", "extern", "import", "struct",
    "if", "else", "while", "break", "next", "enum", "case",
];

string[] imported_files;

enum TokenType {
    EOF,
    WORD,
    KEYWORD,
    OPERATOR,
    NUMBER,
    STRING,
}

struct Token {
    TokenType type = TokenType.EOF;
    string value;
    Loc loc = Loc("", 1);

    string get()
    {
        return format("%s: %s %s", loc.get, to!string(type), value);
    }

    bool opEquals(Token other)
    {
        return (type == other.type && value == other.value);
    }
}

struct Lexer {
    Token[] tokens;
    Loc loc;
    string text;
    ulong pos;

    bool inbound() { return pos < text.length; }
    char current() { return text[pos]; }
}

void skipSpace(Lexer *lex)
{
    // skip whitespace
    while (lex.inbound && lex.current.isWhite) {
        if (lex.current == '\n')
            lex.loc.line++;
        lex.pos++;
    }

    // skip single-line comments
    if (lex.pos + 1 < lex.text.length && lex.text[lex.pos..lex.pos+2] == "//") {
        while (lex.inbound && lex.current != '\n') lex.pos++;
        skipSpace(lex);
    }

    // skip multi-line comments
    if (lex.pos + 1 < lex.text.length && lex.text[lex.pos..lex.pos+2] == "/*") {
        while (lex.pos + 2 < lex.text.length && lex.text[lex.pos..lex.pos+2] != "*/") {
            if (lex.current == '\n')
                lex.loc.line++;
            lex.pos++;
        }
        lex.pos += 2; // skip '*/'
        skipSpace(lex);
    }
}

// TODO: '>>', '<<'
bool isOperator(string s)
{
    return [
        "(", ")", "{", "}", "[", "]", "=>", "|>",
        ":", ".", ",", "&", "|", "!", "=",
        ">", "<", "==", "!=", ">=", "<=",
        "+", "-", "*", "/", "%", "&&", "||",

        "+=", "-=", "*=", "/=", "%=",
    ].canFind(s);
}

Token nextToken(Lexer *lex)
{
    Token tok;
    skipSpace(lex);
    tok.loc = lex.loc;
    if (!lex.inbound)
        return tok;

    // TODO: binary numbers (0b prefix)
    if (lex.current.isDigit) {
        tok.type = TokenType.NUMBER;
        tok.value ~= lex.current;
        lex.pos++;

        if (lex.inbound && (lex.current == 'x' || lex.current == 'X')) {
            tok.value ~= lex.current;
            lex.pos++;
            while (lex.inbound && lex.current.isHexDigit) {
                tok.value ~= lex.current;
                lex.pos++;
            }
            tok.value = to!string(tok.value[2..$].to!ulong(16));
        }
        else {
            while (lex.inbound && lex.current.isDigit || lex.current == '.') {
                tok.value ~= lex.current;
                lex.pos++;
            }
        }
        if (tok.value[$-1] == '.') {
            lex.pos--;
            tok.value.popBack;
        }
        return tok;
    }

    // TODO: maybe allow for single quote strings
    if (lex.current == '\"') {
        tok.type = TokenType.STRING;
        lex.pos++;
        bool backslash;
        while (lex.inbound && lex.current != '\"') {
            tok.value ~= lex.current;
            if (lex.current == '\\')
                backslash = true;
            lex.pos++;
            if (backslash) {
                tok.value ~= lex.current;
                backslash = false;
                lex.pos++;
            }
        }
        lex.pos++;
        return tok;
    }

    if (lex.current.isAlpha || lex.current == '_') {
        while (lex.inbound && lex.current.isAlphaNum || lex.current == '_') {
            tok.value ~= lex.current;
            lex.pos++;
        }
        tok.type = keywords.canFind(tok.value)?
                TokenType.KEYWORD : TokenType.WORD;
        return tok;
    }

    if ((""~lex.current).isOperator) {
        tok.type = TokenType.OPERATOR;
        while (lex.inbound && (tok.value ~ lex.current).isOperator) {
            tok.value ~= lex.current;
            lex.pos++;
        }
        return tok;
    }

    stderr.writefln("%s: error: unexpected '%c'", tok.loc.get, lex.current);
    exit(1);
    return tok;
}

Token[] tokenizeFile(string path)
{
    if (!path.exists || path.isDir) {
        stderr.writefln("error: could not read file '%s'", path);
        exit(1);
    }
    Token tok;
    Lexer lex;
    lex.loc = Loc(path);
    lex.pos = 0;
    lex.text = readText(path);
    // ignore shebang
    if (lex.text.length && lex.text[0] == '#')
        lex.text = lex.text.split('\n')[1..$].join('\n');

    do {
        tok = nextToken(&lex);
        lex.tokens ~= tok;
    } while (tok.type != TokenType.EOF);

    return lex.tokens;
}

struct Parser {
    Token[] toks;
    ulong pos;
    Expr[] ast;

    bool inbound()
    {
        return (pos < toks.length) && (toks[pos].type != TokenType.EOF);
    }

    Token peek(int i = 0)
    {
        if (!inbound)
            return Token(TokenType.EOF, "<EOF>");
        return toks[pos + i];
    }

    Token consume(TokenType type)
    {
        if (peek.type != type) {
            stderr.writefln("%s: error: unexpected '%s'", peek.loc.get, peek.value);
            exit(1);
        }
        auto tok = peek;
        pos++;
        return tok;
    }
}

string getFileFromPath(string path)
{
    if (path.exists && !path.isDir)
        return path;

    auto env = getenv("PATH").to!string.split(':');
    foreach (p; env) {
        auto full = p ~ '/' ~ path;
        if (full.exists && !full.isDir)
            return full;
    }

    stderr.writefln("error: could not read file '%s'", path);
    exit(1);
    assert(0); // make the compiler shut the fuck up
}

Expr[] parseFile(string path)
{
    Parser par;
    par.toks = tokenizeFile(path.getFileFromPath);

    do {
        par.ast ~= parseExpr(&par);
    } while(par.inbound);

    return par.ast;
}

Expr parseExpr(Parser *par)
{
    Expr res;
    if (par.peek.type == TokenType.KEYWORD)
        res = parseKeyword(par);
    else if (par.peek == Token(TokenType.OPERATOR, "{"))
        res = parseBlock(par);
    else
        res = parseBinaryOp(par);

    auto ops = ["+=", "-=", "*=", "/=", "%="];
    if (par.peek.type == TokenType.OPERATOR && ops.canFind(par.peek.value)) {
        auto loc = par.peek.loc;
        auto op = par.peek.value;
        par.pos++; // skip op
        auto expr = parseExpr(par);
        expr = new ExprBinaryOp(loc, op[0]~"", res, expr);
        // a += b
        if (res.type == ExprType.GET_VARIABLE) {
            auto var = cast(ExprGetVariable)res;
            return new ExprSetVariable(res.loc, var.name, expr);
        }
        // a.foo += b
        else if (res.type == ExprType.GET_STRUCT_FIELD) {
            auto str = cast(ExprGetStructField)res;
            return new ExprSetStructField(res.loc, str.struc, str.field, expr);
        }
        // a[0] += b
        else if (res.type == ExprType.GET_ARRAY_INDEX) {
            auto arr = cast(ExprGetArrayAtIndex)res;
            return new ExprSetArrayAtIndex(res.loc, arr.array, arr.index, expr);
        }
        stderr.writefln("%s: error: unexpected '%s'", loc.get, op);
        exit(1);
    }

    return res;
}

Expr parseKeyword(Parser *par)
{
    switch (par.peek.value) {
    case "import":
        return parseKeywordImport(par);
    case "def":
        return parseKeywordDef(par);
    case "extern":
        return parseKeywordExtern(par);
    case "return":
        return parseKeywordReturn(par);
    case "struct":
        return parseKeywordStruct(par);
    case "case":
        return parseKeywordCase(par);
    case "if":
        return parseKeywordIf(par);
    case "while":
        return parseKeywordWhile(par);
    case "break":
        return parseKeywordBreak(par);
    case "next":
        return parseKeywordNext(par);
    case "enum":
        return parseKeywordEnum(par);
    default:
        stderr.writefln("%s: error: unexpected '%s'",
                par.peek.loc.get, par.peek.value);
        exit(1);
    }
    assert(0); // make the compiler happy
}

string[] parseFunctionArgs(Parser *par)
{
    auto end = Token(TokenType.OPERATOR, ")");
    string[] args;

    if (par.peek != Token(TokenType.OPERATOR, "(")) {
        stderr.writefln("%s: error: expected '(', got '%s'",
                par.peek.loc.get, par.peek.value);
        exit(1);
    }

    if (par.peek(1) != end) {
        do {
            par.pos++;

            if (par.peek.type != TokenType.WORD) {
                stderr.writefln("%s: error: unexpected '%s'",
                        par.peek.loc.get, par.peek.value);
                exit(1);
            }

            args ~= par.peek.value;
            par.pos++;
        } while (par.peek == Token(TokenType.OPERATOR, ","));
    }
    else {
        par.pos++;
    }

    if (par.peek != end) {
        stderr.writefln("%s: error: expected ')', got '%s'",
                par.peek.loc.get, par.peek.value);
        exit(1);
    }
    par.pos++; // skip ')'
    return args;
}

Expr parseKeywordImport(Parser *par)
{
    auto loc = par.peek.loc;
    par.pos++; // skip 'import'
    if (par.peek.type != TokenType.STRING) {
        stderr.writefln("%s: error: unexpected '%s'",
                par.peek.loc.get, par.peek.value);
        exit(1);
    }

    auto path = par.peek.value;
    par.pos++;

    if (imported_files.canFind(path))
        return new ExprLiteral(loc, Value(0));

    imported_files ~= path;
    auto expr = parseFile(path);
    return new ExprBlock(loc, expr);
}

Expr parseKeywordDef(Parser *par)
{
    auto loc = par.peek.loc;
    par.pos++; // skip 'def'
    string name;
    if (par.peek.type == TokenType.WORD)
        name = par.consume(TokenType.WORD).value;

    string[] args = parseFunctionArgs(par);
    Expr expr = parseExpr(par);

    return new ExprMakeFunction(loc, name, args, expr);
}

Expr parseKeywordExtern(Parser *par)
{
    auto loc = par.peek.loc;
    par.pos++; // skip 'extern'
    string lib = par.consume(TokenType.STRING).value;
    ExternFuncAlias[] funs;

    if (par.peek != Token(TokenType.OPERATOR, "{")) {
        stderr.writefln("%s: error: unexpected '%s'",
                par.peek.loc.get, par.peek.value);
        exit(1);
    }
    par.pos++; // skip '{'
    while (par.peek != Token(TokenType.OPERATOR, "}")) {
        string name = par.consume(TokenType.WORD).value;
        string func = name;
        string[] types;
        if (par.peek == Token(TokenType.OPERATOR, "=")) {
            par.pos++;
            func = par.consume(TokenType.WORD).value;
        }
        if (par.peek == Token(TokenType.OPERATOR, "(")) {
            types = parseExternTypes(par);
        }
        funs ~= ExternFuncAlias(name, func, types);
    }

    par.pos++; // skip '}'
    return new ExprExtern(loc, lib, funs);
}

string[] parseExternTypes(Parser *par)
{
    auto end = Token(TokenType.OPERATOR, ")");
    par.pos++; // skip '('
    string[] types;

    if (par.peek != end) {
        types ~= par.consume(TokenType.WORD).value;
        while (par.peek == Token(TokenType.OPERATOR, ",")) {
            par.pos++; // skip ','
            auto type = par.consume(TokenType.WORD).value;
            types ~= type;
        }
    }

    if (par.peek != end) {
        stderr.writefln("%s: error: expected ')', got '%s'",
                par.peek.loc.get, par.peek.value);
        exit(1);
    }
    par.pos++; // skip ')'
    return types;
}

Expr parseKeywordReturn(Parser *par)
{
    auto loc = par.peek.loc;
    par.pos++; // skip 'return'
    Expr ret = parseExpr(par);
    return new ExprReturn(loc, ret);
}

Expr parseKeywordStruct(Parser *par)
{
    auto loc = par.peek.loc;
    par.pos++; // skip 'struct'

    string name;
    Expr[string] fields;

    if (par.peek.type != TokenType.WORD) {
        stderr.writefln("%s: error: unexpected '%s'",
                par.peek.loc.get, par.peek.value);
        exit(1);
    }

    name = par.peek.value;
    par.pos++;
    fields = parseStructFields(par);
    return new ExprMakeStruct(loc, name, fields);
}

Expr[string] parseStructFields(Parser *par)
{
    auto end = Token(TokenType.OPERATOR, "}");
    Expr[string] fields;
    Expr[] field_names;

    if (par.peek != Token(TokenType.OPERATOR, "{")) {
        stderr.writefln("%s: error: expected '{', got '%s'",
                par.peek.loc.get, par.peek.value);
        exit(1);
    }
    par.pos++; // skip '{'

    while (par.peek != end) {
        if (par.peek == Token(TokenType.KEYWORD, "def")) {
            auto expr = parseExpr(par);
            if (!(cast(ExprMakeFunction)expr).name) {
                stderr.writefln("%s: error: missing field name",
                        par.peek.loc.get);
                exit(1);
            }
            auto name = (cast(ExprMakeFunction)expr).name;
            field_names ~= new ExprLiteral(par.peek.loc, Value(name));
            (cast(ExprMakeFunction)expr).name = "";
            fields[name] = expr;
            continue;
        }
        if (par.peek.type != TokenType.WORD) {
            stderr.writefln("%s: error: unexpected '%s'",
                    par.peek.loc.get, par.peek.value);
            exit(1);
        }
        if (par.peek.value in fields) {
            stderr.writefln("%s: error: redefinition of field '%s'",
                    par.peek.loc.get, par.peek.value);
            exit(1);
        }

        auto name = par.peek.value;
        field_names ~= new ExprLiteral(par.peek.loc, Value(name));
        fields[name] = new ExprLiteral(par.peek.loc, Value(0));
        par.pos++;
        if (par.peek == Token(TokenType.OPERATOR, "=")) {
            par.pos++; // skip '='
            fields[name] = parseExpr(par);
        }
    }

    fields["@fields"] = new ExprMakeArray(par.peek.loc, field_names);

    if (par.peek != end) {
        stderr.writefln("%s: error: expected '}', got '%s'",
                par.peek.loc.get, par.peek.value);
        exit(1);
    }
    par.pos++; // skip '}'
    return fields;
}

Expr parseKeywordCase(Parser *par)
{
    auto loc = par.peek.loc;
    par.pos++; // skip 'case'
    Expr check = parseExpr(par);
    Match[] matches;
    Expr defalt;
    bool has_defalt;

    auto end = Token(TokenType.OPERATOR, "}");
    if (par.peek != Token(TokenType.OPERATOR, "{")) {
        stderr.writefln("%s: error: expected '{', got '%s'",
                par.peek.loc.get, par.peek.value);
        exit(1);
    }
    par.pos++; // skip '{'
    while (par.inbound && par.peek != end) {
        if (par.peek == Token(TokenType.KEYWORD, "else")) {
            if (par.peek(1) != Token(TokenType.OPERATOR, "=>")) {
                stderr.writefln("%s: error: expected '=>', got '%s'",
                        par.peek.loc.get, par.peek.value);
                exit(1);
            }
            par.pos += 2; // skip 'else =>'
            defalt = parseExpr(par);
            has_defalt = true;
            continue;
        }
        Match match;
        match.match = parseExpr(par);
        if (par.peek != Token(TokenType.OPERATOR, "=>")) {
            stderr.writefln("%s: error: expected '=>', got '%s'",
                    par.peek.loc.get, par.peek.value);
            exit(1);
        }
        par.pos++; // skip '=>'
        match.expr = parseExpr(par);
        matches ~= match;
    }

    if (!par.inbound || par.peek != end) {
        stderr.writefln("%s: error: missing '}'", loc.get);
        exit(1);
    }
    par.pos++; // skip '}'

    return new ExprCase(loc, check, matches, defalt, has_defalt);
}

Expr parseKeywordIf(Parser *par)
{
    auto loc = par.peek.loc;
    par.pos++; // skip 'if'
    Expr cond = parseExpr(par); // condition
    Expr expr = parseExpr(par); // body
    Expr elze;
    bool has_elze;

    if (par.peek == Token(TokenType.KEYWORD, "else")) {
        par.pos++; // skip 'else'
        elze = parseExpr(par); // else body
        has_elze = true;
    }

    return new ExprIf(loc, cond, expr, elze, has_elze);
}

Expr parseKeywordWhile(Parser *par)
{
    auto loc = par.peek.loc;
    par.pos++; // skip 'while'
    Expr cond = parseExpr(par); // condition
    Expr after;
    if (par.peek == Token(TokenType.OPERATOR, ":")) {
        par.pos++; // skip ':'
        after = parseExpr(par);
    }

    Expr expr = parseExpr(par); // body
    return new ExprWhile(loc, cond, expr, after);
}

Expr parseKeywordBreak(Parser *par)
{
    auto loc = par.peek.loc;
    par.pos++; // skip 'break'
    return new ExprBreak(loc, false);
}

Expr parseKeywordNext(Parser *par)
{
    auto loc = par.peek.loc;
    par.pos++; // skip 'next'
    return new ExprBreak(loc, true);
}

Expr parseKeywordEnum(Parser *par) {
    auto loc = par.peek.loc;
    par.pos++; // skip 'enum'

    auto end = Token(TokenType.OPERATOR, "}");
    string[] names;
    Expr[] vars;

    if (par.peek != Token(TokenType.OPERATOR, "{")) {
        stderr.writefln("%s: error: expected '{', got '%s'",
                par.peek.loc.get, par.peek.value);
        exit(1);
    }
    par.pos++; // skip '{'

    uint count = 0;
    while (par.inbound && par.peek != end) {
        if (par.peek.type != TokenType.WORD) {
            stderr.writefln("%s: error: unexpected '%s'",
                    par.peek.loc.get, par.peek.value);
            exit(1);
        }
        if (names.canFind(par.peek.value)) {
            stderr.writefln("%s: error: redefinition of field '%s'",
                    par.peek.loc.get, par.peek.value);
            exit(1);
        }

        loc = par.peek.loc;
        auto name = par.peek.value;
        names ~= name;

        Expr curr = new ExprLiteral(loc, Value(count));
        count++;

        par.pos++;
        if (par.peek == Token(TokenType.OPERATOR, "=")) {
            par.pos++;
            // can only have number literals here coz im lazy
            // and don't wanna have to deal with this bullshit
            curr = parseFactorExtra(par);
            auto lit = cast(ExprLiteral) curr;
            if (curr.type != ExprType.LITERAL || lit.value.type != Type.NUMBER) {
                stderr.writefln("%s: error: expected number literal", loc.get);
                exit(1);
            }
            count += lit.value.value.as_num.to!uint;
        }
        auto var = new ExprSetVariable(loc, name, curr);
        vars ~= var;
    }

    if (!par.inbound || par.peek != end) {
        stderr.writefln("%s: error: missing '}'", loc.get);
        exit(1);
    }

    par.pos++; // skip '}'
    return new ExprMakeEnum(loc, vars);
}

Expr parseBlock(Parser *par)
{
    Expr[] block;
    auto end = Token(TokenType.OPERATOR, "}");
    auto loc = par.peek.loc;
    par.pos++; // skip '{'

    while (par.inbound && par.peek != end) {
        block ~= parseExpr(par);
    }

    if (!par.inbound || par.peek != end) {
        stderr.writefln("%s: error: missing '}'", loc.get);
        exit(1);
    }

    par.pos++; // skip '}'
    return new ExprBlock(loc, block);
}

Expr parseUnaryOp(Parser *par)
{
    auto loc = par.peek.loc;
    auto op = par.peek.value;
    par.pos++;
    Expr expr = parseExpr(par);
    return new ExprUnaryOp(loc, op, expr);
}

Expr parseBinaryOp(Parser *par)
{
    auto loc = par.peek.loc;
    auto ops = ["&&", "||"];
    string op;
    Expr left = parseCondition(par);

    while (par.peek.type == TokenType.OPERATOR && ops.canFind(par.peek.value)) {
        op = par.peek.value;
        par.pos++;

        Expr right = parseCondition(par);
        left = new ExprBinaryOp(loc, op, left, right);
    }

    return left;
}

Expr parseCondition(Parser *par)
{
    auto loc = par.peek.loc;
    auto ops = ["==", "!=", ">", "<", ">=", "<="];
    string op;
    Expr left = parseAddition(par);

    while (par.peek.type == TokenType.OPERATOR && ops.canFind(par.peek.value)) {
        op = par.peek.value;
        par.pos++;

        Expr right = parseAddition(par);
        left = new ExprBinaryOp(loc, op, left, right);
    }

    return left;
}

Expr parseAddition(Parser *par)
{
    auto loc = par.peek.loc;
    auto ops = ["+", "-"];
    string op;
    Expr left = parseMultiplication(par);

    while (par.peek.type == TokenType.OPERATOR && ops.canFind(par.peek.value)) {
        op = par.peek.value;
        par.pos++;

        Expr right = parseMultiplication(par);
        left = new ExprBinaryOp(loc, op, left, right);
    }

    return left;
}

Expr parseMultiplication(Parser *par)
{
    auto loc = par.peek.loc;
    auto ops = ["*", "/", "%"];
    string op;
    Expr left = parseFactorPipe(par);

    while (par.peek.type == TokenType.OPERATOR && ops.canFind(par.peek.value)) {
        op = par.peek.value;
        par.pos++;

        Expr right = parseFactorPipe(par);
        left = new ExprBinaryOp(loc, op, left, right);
    }

    return left;
}

Expr parseFactorPipe(Parser *par)
{
    auto expr = parseFactorExtra(par);

    while (par.peek == Token(TokenType.OPERATOR, "|>")) {
        auto loc = par.peek.loc;
        par.pos++; // skip '|>'
        auto func = parseFactorExtra(par);
        if (func.type != ExprType.CALL_FUNCTION) {
            stderr.writefln("%s: error: unexpected '|>'", loc.get);
            exit(1);
        }
        (cast(ExprCallFunction)func).args = expr ~ (cast(ExprCallFunction)func).args;
        expr = func;
    }
    return expr;
}

Expr parseFactorExtra(Parser *par)
{
    auto expr = parseFactor(par);

    auto rou = Token(TokenType.OPERATOR, "(");
    auto bra = Token(TokenType.OPERATOR, "[");
    auto dot = Token(TokenType.OPERATOR, ".");
    while ([rou, bra, dot].canFind(par.peek)) {
        if (par.peek == dot)
            expr = parseStructField(par, expr);
        if (par.peek == bra)
            expr = parseArrayAtIndex(par, expr);
        if (par.peek == rou)
            expr = parseFunctionCall(par, expr);
    }

    return expr;
}

Expr parseStructField(Parser *par, Expr expr)
{
    auto loc = par.peek.loc;
    par.pos++; // skip '.'
    if (par.peek.type != TokenType.WORD) {
        stderr.writefln("%s: error: unexpected '%s'",
                par.peek.loc.get, par.peek.value);
        exit(1);
    }
    auto field = par.peek.value;
    par.pos++;

    if (par.peek == Token(TokenType.OPERATOR, "=")) {
        par.pos++; // skip '='
        Expr value = parseExpr(par);
        return new ExprSetStructField(loc, expr, field, value);
    }

    return new ExprGetStructField(loc, expr, field);
}

Expr parseFactor(Parser *par)
{
    if (par.peek.type == TokenType.NUMBER)
        return parseNumber(par);
    else if (par.peek.type == TokenType.STRING)
        return parseString(par);
    else if (par.peek.type == TokenType.WORD)
        return parseWord(par);
    else if (par.peek == Token(TokenType.OPERATOR, "["))
        return parseArray(par);
    else if (par.peek == Token(TokenType.OPERATOR, "(")) {
        par.pos++;
        Expr expr = parseExpr(par);
        if (par.peek != Token(TokenType.OPERATOR, ")")) {
            stderr.writefln("%s: error: expected ')', got '%s'",
                    par.peek.loc.get, par.peek.value);
            exit(1);
        }
        par.pos++;
        return expr;
    }
    else if (par.peek.type == TokenType.OPERATOR &&
            ["!", "-"].canFind(par.peek.value)) {
        return parseUnaryOp(par);
    }

    stderr.writefln("%s: error: unexpected '%s'",
            par.peek.loc.get, par.peek.value);
    exit(1);
    assert(0); // make the compiler shut the fuck up
}

Expr parseNumber(Parser *par)
{
    auto loc = par.peek.loc;
    auto val = Value(to!double(par.peek.value));
    par.pos++;
    return new ExprLiteral(loc, val);
}

Expr parseString(Parser *par)
{
    auto loc = par.peek.loc;
    auto val = Value(par.peek.value.escape);
    par.pos++;
    return new ExprLiteral(loc, val);
}

Expr parseArray(Parser *par)
{
    auto loc = par.peek.loc;
    auto end = Token(TokenType.OPERATOR, "]");
    par.pos++; // skip '['
    Expr[] expr;

    if (par.peek != end) {
        expr ~= parseExpr(par);

        while (par.peek == Token(TokenType.OPERATOR, ",")) {
            par.pos++; // skip ','
            if (par.peek == end)
                break;
            expr ~= parseExpr(par);
        }
    }

    if (par.peek != end) {
        stderr.writefln("%s: error: expected ']', got '%s'",
                par.peek.loc.get, par.peek.value);
        exit(1);
    }

    par.pos++; // skip ']'
    return new ExprMakeArray(loc, expr);
}

Expr parseArrayAtIndex(Parser *par, Expr array)
{
    auto loc = par.peek.loc;
    par.pos++; // skip '['
    Expr index = parseExpr(par);

    if (par.peek != Token(TokenType.OPERATOR, "]")) {
        stderr.writefln("%s: error: expected ']', got '%s'",
                par.peek.loc.get, par.peek.value);
        exit(1);
    }
    par.pos++; // skip ']'

    if (par.peek == Token(TokenType.OPERATOR, "=")) {
        par.pos++; // skip '='
        Expr expr = parseExpr(par);
        return new ExprSetArrayAtIndex(loc, array, index, expr);
    }

    return new ExprGetArrayAtIndex(loc, array, index);
}

Expr parseWord(Parser *par)
{
    auto loc = par.peek.loc;
    auto name = par.peek.value;
    par.pos++;

    if (par.peek == Token(TokenType.OPERATOR, "="))
        return parseAssignment(par, name);

    return new ExprGetVariable(loc, name);
}

Expr parseAssignment(Parser *par, string name)
{
    auto loc = par.peek.loc;
    par.pos++; // skip '='
    Expr expr = parseExpr(par);
    return new ExprSetVariable(loc, name, expr);
}

Expr parseFunctionCall(Parser *par, Expr func)
{
    auto loc = par.peek.loc;
    auto end = Token(TokenType.OPERATOR, ")");
    par.pos++; // skip '('
    Expr[] args;

    if (par.peek != end) {
        args ~= parseExpr(par);

        while (par.peek == Token(TokenType.OPERATOR, ",")) {
            par.pos++; // skip ','
            args ~= parseExpr(par);
        }
    }

    if (par.peek != end) {
        stderr.writefln("%s: error: expected ')', got '%s'",
                par.peek.loc.get, par.peek.value);
        exit(1);
    }
    par.pos++; // skip ')'

    return new ExprCallFunction(loc, func, args);
}

