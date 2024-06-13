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
    "if", "else", "while", "break", "next",
];

struct Loc {
    string path;
    ulong line = 1;

    string get()
    {
        return format("%s:%d", path, line);
    }
}

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

bool isOperator(string s)
{
    return [
        "(", ")", "{", "}", "[", "]",
        ":", ".", ",", "&", "|", "!", "=",
        ">", "<", "==", "!=", ">=", "<=",
        "+", "-", "*", "/", "&&", "||",
    ].canFind(s);
}

Token nextToken(Lexer *lex)
{
    Token tok;
    tok.loc = lex.loc;
    skipSpace(lex);
    if (!lex.inbound)
        return tok;

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

    if (lex.current.isAlpha) {
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
}

Expr[] parseFile(string path)
{
    Parser par;
    par.toks = tokenizeFile(path);

    do {
        par.ast ~= parseExpr(&par);
    } while(par.inbound);

    return par.ast;
}

Expr parseExpr(Parser *par)
{
    if (par.peek.type == TokenType.KEYWORD)
        return parseKeyword(par);
    if (par.peek == Token(TokenType.OPERATOR, "{"))
        return parseBlock(par);
    if (par.peek == Token(TokenType.OPERATOR, "!") ||
            par.peek == Token(TokenType.OPERATOR, "-"))
        return parseUnaryOp(par);

    return parseBinaryOp(par);
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
    case "if":
        return parseKeywordIf(par);
    case "while":
        return parseKeywordWhile(par);
    case "break":
        return parseKeywordBreak(par);
    case "next":
        return parseKeywordNext(par);
    default:
        stderr.writefln("%s: error: unexpected keyword '%s'",
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
    par.pos++; // skip 'import'
    if (par.peek.type != TokenType.STRING) {
        stderr.writefln("%s: error: unexpected '%s'",
                par.peek.loc.get, par.peek.value);
        exit(1);
    }

    auto path = par.peek.value;
    par.pos++;

    auto expr = parseFile(path);
    return new ExprBlock(expr);
}

Expr parseKeywordDef(Parser *par)
{
    par.pos++; // skip 'def'
    if (par.peek.type != TokenType.WORD) {
        stderr.writefln("%s: error: unexpected '%s'",
                par.peek.loc.get, par.peek.value);
        exit(1);
    }

    auto name = par.peek.value;
    par.pos++;
    string[] args = parseFunctionArgs(par);
    Expr expr = parseExpr(par);

    return new ExprMakeFunction(name, args, expr);
}

Expr parseKeywordExtern(Parser *par)
{
    par.pos++; // skip 'extern'
    if (par.peek.type != TokenType.STRING) {
        stderr.writefln("%s: error: expected string, got %s",
                par.peek.loc.get, par.peek.value);
        exit(1);
    }
    
    string lib = par.peek.value;
    ExternFuncAlias[] funs;
    par.pos++;

    if (par.peek == Token(TokenType.OPERATOR, "(")) {
        par.pos++; // skip '('
        while (par.peek != Token(TokenType.OPERATOR, ")")) {
            if (par.peek.type != TokenType.WORD) {
                stderr.writefln("%s: error: unexpected %s",
                        par.peek.loc.get, par.peek.value);
                exit(1);
            }

            string name = par.peek.value;
            string func = name;
            par.pos++;
            if (par.peek == Token(TokenType.OPERATOR, "=")) {
                par.pos++;
                if (par.peek.type != TokenType.WORD) {
                    stderr.writefln("%s: error: unexpected %s",
                            par.peek.loc.get, par.peek.value);
                    exit(1);
                }
                func = par.peek.value;
                par.pos++;
            }
            funs ~= ExternFuncAlias(name, func);
        }
        par.pos++; // skip ')'
    }
    else if (par.peek.type == TokenType.WORD) {
        string name = par.peek.value;
        string func = name;
        par.pos++;
        if (par.peek == Token(TokenType.OPERATOR, "=")) {
            par.pos++;
            if (par.peek.type != TokenType.WORD) {
                stderr.writefln("%s: error: unexpected %s",
                        par.peek.loc.get, par.peek.value);
                exit(1);
            }
            func = par.peek.value;
            par.pos++;
        }
        funs ~= ExternFuncAlias(name, func);
    }
    else {
        stderr.writefln("%s: error: unexpected %s",
                par.peek.loc.get, par.peek.value);
        exit(1);
    }

    return new ExprExtern(lib, funs);
}

Expr parseKeywordReturn(Parser *par)
{
    auto line = par.peek.loc.line;
    par.pos++; // skip 'return'
    Expr ret = parseExpr(par);
    return new ExprReturn(ret);
}

Expr parseKeywordStruct(Parser *par)
{
    par.pos++; // skip 'struct'

    string name;
    string[] fields;

    if (par.peek.type != TokenType.WORD) {
        stderr.writefln("%s: error: unexpected %s",
                par.peek.loc.get, par.peek.value);
        exit(1);
    }

    name = par.peek.value;
    par.pos++;
    fields = parseStructFields(par);
    return new ExprMakeStruct(name, fields);
}

string[] parseStructFields(Parser *par)
{
    auto end = Token(TokenType.OPERATOR, "}");
    string[] fields;

    if (par.peek != Token(TokenType.OPERATOR, "{")) {
        stderr.writefln("%s: error: expected '{', got %s",
                par.peek.loc.get, par.peek.value);
        exit(1);
    }
    par.pos++; // skip '{'

    while (par.peek != end) {
        if (par.peek.type != TokenType.WORD) {
            stderr.writefln("%s: error: unexpected %s",
                    par.peek.loc.get, par.peek.value);
            exit(1);
        }
        fields ~= par.peek.value;
        par.pos++;
    }

    if (par.peek != end) {
        stderr.writefln("%s: error: expected '}', got %s",
                par.peek.loc.get, par.peek.value);
        exit(1);
    }
    par.pos++; // skip '}'
    return fields;
}

Expr parseKeywordIf(Parser *par)
{
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

    return new ExprIf(cond, expr, elze, has_elze);
}

Expr parseKeywordWhile(Parser *par)
{
    par.pos++; // skip 'while'
    Expr cond = parseExpr(par); // condition
    Expr after;
    if (par.peek == Token(TokenType.OPERATOR, ":")) {
        par.pos++; // skip ':'
        after = parseExpr(par);
    }

    Expr expr = parseExpr(par); // body
    return new ExprWhile(cond, expr, after);
}

Expr parseKeywordBreak(Parser *par)
{
    par.pos++; // skip 'break'
    return new ExprBreak(false);
}

Expr parseKeywordNext(Parser *par)
{
    par.pos++; // skip 'next'
    return new ExprBreak(true);
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
        stderr.writefln("%s: error: missing '}'", loc);
    }

    par.pos++; // skip '}'
    return new ExprBlock(block);
}

Expr parseUnaryOp(Parser *par)
{
    string op = par.peek.value;
    par.pos++;
    Expr expr = parseExpr(par);
    return new ExprUnaryOp(op, expr);
}

Expr parseBinaryOp(Parser *par)
{
    auto ops = ["&&", "||"];
    string op;
    Expr left = parseCondition(par);

    while (par.peek.type == TokenType.OPERATOR && ops.canFind(par.peek.value)) {
        op = par.peek.value;
        par.pos++;

        Expr right = parseCondition(par);
        left = new ExprBinaryOp(op, left, right);
    }

    return left;
}

Expr parseCondition(Parser *par)
{
    auto ops = ["==", "!=", ">", "<", ">=", "<="];
    string op;
    Expr left = parseAddition(par);

    while (par.peek.type == TokenType.OPERATOR && ops.canFind(par.peek.value)) {
        op = par.peek.value;
        par.pos++;

        Expr right = parseAddition(par);
        left = new ExprBinaryOp(op, left, right);
    }

    return left;
}

Expr parseAddition(Parser *par)
{
    auto ops = ["+", "-"];
    string op;
    Expr left = parseMultiplication(par);

    while (par.peek.type == TokenType.OPERATOR && ops.canFind(par.peek.value)) {
        op = par.peek.value;
        par.pos++;

        Expr right = parseMultiplication(par);
        left = new ExprBinaryOp(op, left, right);
    }

    return left;
}

Expr parseMultiplication(Parser *par)
{
    auto ops = ["*", "/"];
    string op;
    Expr left = parseFactorExtra(par);

    while (par.peek.type == TokenType.OPERATOR && ops.canFind(par.peek.value)) {
        op = par.peek.value;
        par.pos++;

        Expr right = parseFactorExtra(par);
        left = new ExprBinaryOp(op, left, right);
    }

    return left;
}

Expr parseFactorExtra(Parser *par)
{
    auto expr = parseFactor(par);

    auto bra = Token(TokenType.OPERATOR, "[");
    auto dot = Token(TokenType.OPERATOR, ".");
    while ([bra, dot].canFind(par.peek)) {
        if (par.peek == dot)
            expr = parseStructField(par, expr);
        if (par.peek == bra)
            expr = parseArrayAtIndex(par, expr);
    }

    return expr;
}

Expr parseStructField(Parser *par, Expr expr)
{
    par.pos++; // skip '.'
    if (par.peek.type != TokenType.WORD) {
        stderr.writefln("%s: error: unexpected %s",
                par.peek.loc.get, par.peek.value);
        exit(1);
    }
    auto field = par.peek.value;
    par.pos++;

    if (par.peek == Token(TokenType.OPERATOR, "=")) {
        par.pos++; // skip '='
        Expr value = parseExpr(par);
        return new ExprSetStructField(expr, field, value);
    }

    return new ExprGetStructField(expr, field);
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

    stderr.writefln("%s: error: unexpected '%s'",
            par.peek.loc.get, par.peek.value);
    exit(1);
}

Expr parseNumber(Parser *par)
{
    auto val = Value(to!double(par.peek.value));
    par.pos++;
    return new ExprLiteral(val);
}

Expr parseString(Parser *par)
{
    auto val = Value(par.peek.value.escape);
    par.pos++;
    return new ExprLiteral(val);
}

Expr parseArray(Parser *par)
{
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
    return new ExprMakeArray(expr);
}

Expr parseArrayAtIndex(Parser *par, Expr array)
{
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
        return new ExprSetArrayAtIndex(array, index, expr);
    }

    return new ExprGetArrayAtIndex(array, index);
}

Expr parseWord(Parser *par)
{
    auto name = par.peek.value;
    par.pos++;

    if (par.peek == Token(TokenType.OPERATOR, "="))
        return parseAssignment(par, name);
    if (par.peek == Token(TokenType.OPERATOR, "("))
        return parseFunctionCall(par, name);

    return new ExprGetVariable(name);
}

Expr parseAssignment(Parser *par, string name)
{
    par.pos++; // skip '='
    Expr expr = parseExpr(par);
    return new ExprSetVariable(name, expr);
}

Expr parseFunctionCall(Parser *par, string name)
{
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

    return new ExprCallFunction(name, args);
}
