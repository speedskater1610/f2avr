#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"
#include "token.h"
#include <string>
#include <map>
#include <sstream>

class Parser {
private:
    Lexer lexer;
    Token currentToken;
    std::stringstream output;
    std::map<std::string, std::string> varTypes;
    int indentLevel;
    
    void indent();
    void advance();
    void expect(TokenType type);
    
    void parseProgram();
    void parseDeclarations();
    void parseStatements();
    void parseStatement();
    void parseAssignment();
    void parseIf();
    void parseDoWhile();
    void parsePrint();
    void parseCall();
    void parsePinMode();
    void parseDigitalWrite();
    void parseDigitalRead();
    void parseDelay();
    void parseExpression();
    void parseTerm();
    void parseFactor();
    
public:
    Parser(const std::string& source);
    std::string compile();
};

#endif // PARSER_H

