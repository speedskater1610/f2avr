#include "lexer.h"
#include <cctype>
#include <algorithm>
#include <iostream>

Lexer::Lexer(const std::string& src) : source(src), pos(0), line(1) {}

char Lexer::peek() {
    if (pos >= source.length()) return '\0';
    return source[pos];
}

char Lexer::advance() {
    if (pos >= source.length()) return '\0';
    return source[pos++];
}

void Lexer::skipWhitespace() {
    while (peek() == ' ' || peek() == '\t' || peek() == '\r') {
        advance();
    }
}

void Lexer::skipComment() {
    if (peek() == '!') {
        while (peek() != '\n' && peek() != '\0') {
            advance();
        }
    }
}

std::string Lexer::readIdentifier() {
    std::string id;
    while (isalnum(peek()) || peek() == '_') {
        id += advance();
    }
    return id;
}

std::string Lexer::readNumber() {
    std::string num;
    while (isdigit(peek()) || peek() == '.') {
        num += advance();
    }
    return num;
}

std::string Lexer::readString() {
    std::string str;
    char quote = advance();
    while (peek() != quote && peek() != '\0') {
        str += advance();
    }
    if (peek() == quote) advance();
    return str;
}

Token Lexer::nextToken() {
    skipWhitespace();
    skipComment();
    
    Token tok;
    tok.line = line;
    
    char c = peek();
    
    if (c == '\0') {
        tok.type = TOK_EOF;
        return tok;
    }
    
    if (c == '\n') {
        advance();
        line++;
        tok.type = TOK_NEWLINE;
        return tok;
    }
    
    if (c == '\'' || c == '"') {
        tok.type = TOK_STRING;
        tok.value = readString();
        return tok;
    }
    
    if (isalpha(c)) {
        std::string id = readIdentifier();
        std::transform(id.begin(), id.end(), id.begin(), ::toupper);
        
        if (id == "PROGRAM") tok.type = TOK_PROGRAM;
        else if (id == "END") tok.type = TOK_END;
        else if (id == "INTEGER") tok.type = TOK_INTEGER;
        else if (id == "REAL") tok.type = TOK_REAL;
        else if (id == "LOGICAL") tok.type = TOK_LOGICAL;
        else if (id == "IF") tok.type = TOK_IF;
        else if (id == "THEN") tok.type = TOK_THEN;
        else if (id == "ELSE") tok.type = TOK_ELSE;
        else if (id == "ENDIF") tok.type = TOK_ENDIF;
        else if (id == "DO") tok.type = TOK_DO;
        else if (id == "WHILE") tok.type = TOK_WHILE;
        else if (id == "ENDDO") tok.type = TOK_ENDDO;
        else if (id == "PRINT") tok.type = TOK_PRINT;
        else if (id == "READ") tok.type = TOK_READ;
        else if (id == "AND") tok.type = TOK_AND;
        else if (id == "OR") tok.type = TOK_OR;
        else if (id == "NOT") tok.type = TOK_NOT;
        else if (id == "TRUE") tok.type = TOK_TRUE;
        else if (id == "FALSE") tok.type = TOK_FALSE;
        else if (id == "PINMODE") tok.type = TOK_PINMODE;
        else if (id == "DIGITALWRITE") tok.type = TOK_DIGITALWRITE;
        else if (id == "DIGITALREAD") tok.type = TOK_DIGITALREAD;
        else if (id == "DELAY") tok.type = TOK_DELAY;
        else if (id == "INPUT") tok.type = TOK_INPUT;
        else if (id == "OUTPUT") tok.type = TOK_OUTPUT;
        else if (id == "HIGH") tok.type = TOK_HIGH;
        else if (id == "LOW") tok.type = TOK_LOW;
        else if (id == "CALL") tok.type = TOK_CALL;
        else if (id == "SUBROUTINE") tok.type = TOK_SUBROUTINE;
        else if (id == "FUNCTION") tok.type = TOK_FUNCTION;
        else if (id == "RETURN") tok.type = TOK_RETURN;
        else if (id == "GOTO") tok.type = TOK_GOTO;
        else if (id == "CONTINUE") tok.type = TOK_CONTINUE;
        else {
            tok.type = TOK_IDENTIFIER;
            tok.value = id;
        }
        return tok;
    }
    
    if (isdigit(c)) {
        tok.type = TOK_NUMBER;
        tok.value = readNumber();
        return tok;
    }
    
    advance();
    switch (c) {
        case '=':
            if (peek() == '=') {
                advance();
                tok.type = TOK_EQ;
            } else {
                tok.type = TOK_ASSIGN;
            }
            break;
        case '+': tok.type = TOK_PLUS; break;
        case '-': tok.type = TOK_MINUS; break;
        case '*': tok.type = TOK_MULTIPLY; break;
        case '/':
            if (peek() == '=') {
                advance();
                tok.type = TOK_NE;
            } else {
                tok.type = TOK_DIVIDE;
            }
            break;
        case '(': tok.type = TOK_LPAREN; break;
        case ')': tok.type = TOK_RPAREN; break;
        case ',': tok.type = TOK_COMMA; break;
        case '<':
            if (peek() == '=') {
                advance();
                tok.type = TOK_LE;
            } else {
                tok.type = TOK_LT;
            }
            break;
        case '>':
            if (peek() == '=') {
                advance();
                tok.type = TOK_GE;
            } else {
                tok.type = TOK_GT;
            }
            break;
        default:
            std::cerr << "Unknown character: " << c << std::endl;
            tok.type = TOK_EOF;
    }
    
    return tok;
}
