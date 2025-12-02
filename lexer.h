#ifndef LEXER_H
#define LEXER_H

#include "token.h"
#include <string>

class Lexer {
private:
    std::string source;
    size_t pos;
    int line;
    
    char peek();
    char advance();
    void skipWhitespace();
    void skipComment();
    std::string readIdentifier();
    std::string readNumber();
    std::string readString();
    
public:
    Lexer(const std::string& src);
    Token nextToken();
};

#endif // LEXER_H
