#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"
#include "token.h"
#include <string>
#include <map>
#include <vector>
#include <sstream>

struct SubroutineInfo {
    std::string name;
    std::vector<std::string> params;
    std::string returnType;  // Empty for subroutines
};

class Parser {
private:
    Lexer lexer;
    Token currentToken;
    std::stringstream output;
    std::stringstream functionDecls;
    std::map<std::string, std::string> varTypes;
    std::map<std::string, SubroutineInfo> subroutines;
    int indentLevel;
    bool inSubroutine;
    
    void indent();
    void advance();
    void expect(TokenType type);
    
    void parseProgram();
    void parseSubroutine();
    void parseFunction();
    void parseDeclarations();
    void parseStatements();
    void parseStatement();
    void parseAssignment();
    void parseIf();
    void parseDoWhile();
    void parsePrint();
    void parseCall();
    void parseGoto();
    void parseReturn();
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
