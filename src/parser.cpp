#include "parser.h"
#include <iostream>
#include <cstdlib>

Parser::Parser(const std::string& source) : lexer(source), indentLevel(0), inSubroutine(false) {
    advance();
}

void Parser::indent() {
    for (int i = 0; i < indentLevel; i++) {
        output << "  ";
    }
}

void Parser::advance() {
    do {
        currentToken = lexer.nextToken();
    } while (currentToken.type == TOK_NEWLINE);
}

void Parser::expect(TokenType type) {
    if (currentToken.type != type) {
        std::cerr << "Parse error at line " << currentToken.line << std::endl;
        exit(1);
    }
    advance();
}

void Parser::parseProgram() {
    // Parse subroutines and functions first
    while (currentToken.type == TOK_SUBROUTINE || currentToken.type == TOK_FUNCTION) {
        if (currentToken.type == TOK_SUBROUTINE) {
            parseSubroutine();
        } else {
            parseFunction();
        }
    }
    
    expect(TOK_PROGRAM);
    std::string progName = currentToken.value;
    expect(TOK_IDENTIFIER);
    
    output << "// Generated from Fortran program: " << progName << "\n";
    output << "#include <avr/io.h>\n";
    output << "#include <util/delay.h>\n";
    output << "#include <stdint.h>\n\n";
    
    // Add GPIO helper macros
    output << "// GPIO helper macros\n";
    output << "#define SET_OUTPUT(port, pin) DDR##port |= (1 << pin)\n";
    output << "#define SET_INPUT(port, pin) DDR##port &= ~(1 << pin)\n";
    output << "#define SET_HIGH(port, pin) PORT##port |= (1 << pin)\n";
    output << "#define SET_LOW(port, pin) PORT##port &= ~(1 << pin)\n";
    output << "#define READ_PIN(port, pin) ((PIN##port >> pin) & 1)\n\n";
    
    // Add function declarations
    if (!functionDecls.str().empty()) {
        output << "// Forward declarations\n";
        output << functionDecls.str() << "\n";
    }
    
    output << "int main(void) {\n";
    indentLevel++;
    
    parseDeclarations();
    parseStatements();
    
    output << "\n";
    indentLevel--;
    indent();
    output << "return 0;\n";
    output << "}\n";
    
    expect(TOK_END);
}

void Parser::parseDeclarations() {
    std::map<std::string, bool> declaredVars;  // Track declared variables
    
    while (currentToken.type == TOK_INTEGER || 
           currentToken.type == TOK_REAL || 
           currentToken.type == TOK_LOGICAL) {
        
        std::string cType;
        if (currentToken.type == TOK_INTEGER) {
            cType = "int16_t";
            advance();
        } else if (currentToken.type == TOK_REAL) {
            cType = "float";
            advance();
        } else {
            cType = "uint8_t";
            advance();
        }
        
        do {
            std::string varName = currentToken.value;
            
            // Skip if already declared
            if (declaredVars.find(varName) != declaredVars.end()) {
                expect(TOK_IDENTIFIER);
                if (currentToken.type == TOK_COMMA) {
                    advance();
                }
                continue;
            }
            
            varTypes[varName] = cType;
            declaredVars[varName] = true;
            expect(TOK_IDENTIFIER);
            
            indent();
            output << cType << " " << varName << " = 0;\n";
            
            if (currentToken.type == TOK_COMMA) {
                advance();
            }
        } while (currentToken.type == TOK_IDENTIFIER);
    }
    
    if (!varTypes.empty()) {
        output << "\n";
    }
}

void Parser::parseStatements() {
    while (currentToken.type != TOK_END && currentToken.type != TOK_EOF &&
           currentToken.type != TOK_ELSE && currentToken.type != TOK_ENDIF &&
           currentToken.type != TOK_ENDDO && currentToken.type != TOK_SUBROUTINE &&
           currentToken.type != TOK_FUNCTION) {
        parseStatement();
    }
}

void Parser::parseStatement() {
    // Check for labels (numbers followed by CONTINUE or as standalone statement)
    // A label must be at the beginning of a statement, not after an assignment operator
    if (currentToken.type == TOK_NUMBER) {
        // Peek ahead to see if this is really a label
        // Save current position
        Token saved = currentToken;
        advance();
        
        // If followed by CONTINUE or another statement keyword, it's a label
        if (currentToken.type == TOK_CONTINUE || 
            currentToken.type == TOK_IF ||
            currentToken.type == TOK_DO ||
            currentToken.type == TOK_CALL ||
            currentToken.type == TOK_GOTO ||
            currentToken.type == TOK_IDENTIFIER) {
            // It's a label - restore and parse it
            currentToken = saved;
            // Re-advance to get past the number
            std::string label = currentToken.value;
            expect(TOK_NUMBER);
            
            // Decrease indent for label
            indentLevel--;
            indent();
            output << "label_" << label << ":;\n";
            indentLevel++;
            
            if (currentToken.type == TOK_CONTINUE) {
                advance();
            }
            return;
        } else {
            // Not a label, restore and continue
            // This shouldn't happen in valid code - numbers alone aren't valid statements
            currentToken = saved;
            advance();
            return;
        }
    }
    
    if (currentToken.type == TOK_IDENTIFIER) {
        parseAssignment();
    } else if (currentToken.type == TOK_IF) {
        parseIf();
    } else if (currentToken.type == TOK_DO) {
        parseDoWhile();
    } else if (currentToken.type == TOK_PRINT) {
        parsePrint();
    } else if (currentToken.type == TOK_CALL) {
        parseCall();
    } else if (currentToken.type == TOK_GOTO) {
        parseGoto();
    } else if (currentToken.type == TOK_RETURN) {
        parseReturn();
    } else if (currentToken.type == TOK_CONTINUE) {
        advance();  // CONTINUE is just a label target, no code needed
    } else {
        advance();
    }
}

void Parser::parseAssignment() {
    std::string varName = currentToken.value;
    expect(TOK_IDENTIFIER);
    expect(TOK_ASSIGN);
    
    indent();
    output << varName << " = ";
    parseExpression();
    output << ";\n";
}

void Parser::parseIf() {
    expect(TOK_IF);
    expect(TOK_LPAREN);
    
    indent();
    output << "if (";
    parseExpression();
    output << ") {\n";
    
    expect(TOK_RPAREN);
    expect(TOK_THEN);
    
    indentLevel++;
    parseStatements();
    indentLevel--;
    
    if (currentToken.type == TOK_ELSE) {
        advance();
        indent();
        output << "} else {\n";
        indentLevel++;
        parseStatements();
        indentLevel--;
    }
    
    indent();
    output << "}\n";
    expect(TOK_ENDIF);
}

void Parser::parseDoWhile() {
    expect(TOK_DO);
    expect(TOK_WHILE);
    expect(TOK_LPAREN);
    
    indent();
    output << "while (";
    parseExpression();
    output << ") {\n";
    
    expect(TOK_RPAREN);
    
    indentLevel++;
    parseStatements();
    indentLevel--;
    
    indent();
    output << "}\n";
    expect(TOK_ENDDO);
}

void Parser::parsePrint() {
    expect(TOK_PRINT);
    expect(TOK_MULTIPLY);
    expect(TOK_COMMA);
    
    indent();
    output << "// PRINT statement (AVR has no standard output)\n";
    
    while (currentToken.type == TOK_IDENTIFIER || 
           currentToken.type == TOK_NUMBER ||
           currentToken.type == TOK_STRING) {
        advance();
        if (currentToken.type == TOK_COMMA) {
            advance();
        }
    }
}

void Parser::parseCall() {
    expect(TOK_CALL);
    
    // Check for built-in GPIO functions
    if (currentToken.type == TOK_PINMODE) {
        parsePinMode();
    } else if (currentToken.type == TOK_DIGITALWRITE) {
        parseDigitalWrite();
    } else if (currentToken.type == TOK_DIGITALREAD) {
        parseDigitalRead();
    } else if (currentToken.type == TOK_DELAY) {
        parseDelay();
    } else if (currentToken.type == TOK_IDENTIFIER) {
        // User-defined subroutine call
        std::string subName = currentToken.value;
        expect(TOK_IDENTIFIER);
        
        indent();
        output << subName << "(";
        
        if (currentToken.type == TOK_LPAREN) {
            expect(TOK_LPAREN);
            
            bool first = true;
            while (currentToken.type != TOK_RPAREN) {
                if (!first) {
                    output << ", ";
                    expect(TOK_COMMA);
                }
                first = false;
                
                if (currentToken.type == TOK_IDENTIFIER) {
                    output << currentToken.value;
                    advance();
                } else if (currentToken.type == TOK_NUMBER) {
                    output << currentToken.value;
                    advance();
                } else {
                    parseExpression();
                }
            }
            
            expect(TOK_RPAREN);
        }
        
        output << ");\n";
    } else {
        std::cerr << "Unknown subroutine at line " << currentToken.line << std::endl;
        exit(1);
    }
}

void Parser::parseSubroutine() {
    expect(TOK_SUBROUTINE);
    std::string subName = currentToken.value;
    expect(TOK_IDENTIFIER);
    
    SubroutineInfo info;
    info.name = subName;
    info.returnType = "";  // Subroutines don't return values
    
    // Parse parameters
    if (currentToken.type == TOK_LPAREN) {
        expect(TOK_LPAREN);
        
        while (currentToken.type != TOK_RPAREN) {
            std::string paramName = currentToken.value;
            info.params.push_back(paramName);
            expect(TOK_IDENTIFIER);
            
            if (currentToken.type == TOK_COMMA) {
                advance();
            }
        }
        
        expect(TOK_RPAREN);
    }
    
    subroutines[subName] = info;
    
    // Generate function declaration
    functionDecls << "void " << subName << "(";
    for (size_t i = 0; i < info.params.size(); i++) {
        if (i > 0) functionDecls << ", ";
        functionDecls << "int16_t* " << info.params[i];
    }
    functionDecls << ");\n";
    
    // Generate function definition
    output << "\nvoid " << subName << "(";
    for (size_t i = 0; i < info.params.size(); i++) {
        if (i > 0) output << ", ";
        output << "int16_t* " << info.params[i];
    }
    output << ") {\n";
    
    indentLevel++;
    inSubroutine = true;
    
    parseDeclarations();
    parseStatements();
    
    inSubroutine = false;
    indentLevel--;
    output << "}\n";
    
    expect(TOK_END);
}

void Parser::parseFunction() {
    std::string returnType = "int16_t";  // Default
    
    if (currentToken.type == TOK_INTEGER) {
        returnType = "int16_t";
        advance();
    } else if (currentToken.type == TOK_REAL) {
        returnType = "float";
        advance();
    } else if (currentToken.type == TOK_LOGICAL) {
        returnType = "uint8_t";
        advance();
    }
    
    expect(TOK_FUNCTION);
    std::string funcName = currentToken.value;
    expect(TOK_IDENTIFIER);
    
    SubroutineInfo info;
    info.name = funcName;
    info.returnType = returnType;
    
    // Parse parameters
    if (currentToken.type == TOK_LPAREN) {
        expect(TOK_LPAREN);
        
        while (currentToken.type != TOK_RPAREN) {
            std::string paramName = currentToken.value;
            info.params.push_back(paramName);
            expect(TOK_IDENTIFIER);
            
            if (currentToken.type == TOK_COMMA) {
                advance();
            }
        }
        
        expect(TOK_RPAREN);
    }
    
    subroutines[funcName] = info;
    
    // Generate function declaration
    functionDecls << returnType << " " << funcName << "(";
    for (size_t i = 0; i < info.params.size(); i++) {
        if (i > 0) functionDecls << ", ";
        functionDecls << "int16_t " << info.params[i];
    }
    functionDecls << ");\n";
    
    // Generate function definition
    output << "\n" << returnType << " " << funcName << "(";
    for (size_t i = 0; i < info.params.size(); i++) {
        if (i > 0) output << ", ";
        output << "int16_t " << info.params[i];
    }
    output << ") {\n";
    
    indentLevel++;
    inSubroutine = true;
    
    // Function return value variable
    indent();
    output << returnType << " " << funcName << "_result = 0;\n";
    
    parseDeclarations();
    parseStatements();
    
    indent();
    output << "return " << funcName << "_result;\n";
    
    inSubroutine = false;
    indentLevel--;
    output << "}\n";
    
    expect(TOK_END);
}

void Parser::parseGoto() {
    expect(TOK_GOTO);
    std::string label = currentToken.value;
    expect(TOK_NUMBER);
    
    indent();
    output << "goto label_" << label << ";\n";
}

void Parser::parseLabel() {
    std::string label = currentToken.value;
    expect(TOK_NUMBER);
    
    // Decrease indent for label
    if (indentLevel > 0) {
        indentLevel--;
    }
    indent();
    output << "label_" << label << ":;\n";
    indentLevel++;
    
    if (currentToken.type == TOK_CONTINUE) {
        advance();
    }
}

void Parser::parseReturn() {
    expect(TOK_RETURN);
    
    indent();
    if (inSubroutine) {
        output << "return;\n";
    } else {
        output << "return 0;\n";
    }
}

void Parser::parsePinMode() {
    expect(TOK_PINMODE);
    expect(TOK_LPAREN);
    
    // Port letter (B, C, D)
    std::string port = currentToken.value;
    expect(TOK_IDENTIFIER);
    expect(TOK_COMMA);
    
    // Pin number
    std::string pin = currentToken.value;
    expect(TOK_NUMBER);
    expect(TOK_COMMA);
    
    // Mode (INPUT or OUTPUT)
    TokenType mode = currentToken.type;
    if (mode == TOK_INPUT) {
        advance();
        indent();
        output << "SET_INPUT(" << port << ", " << pin << ");\n";
    } else if (mode == TOK_OUTPUT) {
        advance();
        indent();
        output << "SET_OUTPUT(" << port << ", " << pin << ");\n";
    }
    
    expect(TOK_RPAREN);
}

void Parser::parseDigitalWrite() {
    expect(TOK_DIGITALWRITE);
    expect(TOK_LPAREN);
    
    // Port letter
    std::string port = currentToken.value;
    expect(TOK_IDENTIFIER);
    expect(TOK_COMMA);
    
    // Pin number
    std::string pin = currentToken.value;
    expect(TOK_NUMBER);
    expect(TOK_COMMA);
    
    // State (HIGH or LOW)
    TokenType state = currentToken.type;
    if (state == TOK_HIGH) {
        advance();
        indent();
        output << "SET_HIGH(" << port << ", " << pin << ");\n";
    } else if (state == TOK_LOW) {
        advance();
        indent();
        output << "SET_LOW(" << port << ", " << pin << ");\n";
    } else {
        // Variable state
        std::string varName = currentToken.value;
        expect(TOK_IDENTIFIER);
        indent();
        output << "if (" << varName << ") { SET_HIGH(" << port << ", " << pin << "); } ";
        output << "else { SET_LOW(" << port << ", " << pin << "); }\n";
    }
    
    expect(TOK_RPAREN);
}

void Parser::parseDigitalRead() {
    // For assignment: variable = DIGITALREAD(port, pin)
    // This is handled differently - called from parseAssignment
    expect(TOK_DIGITALREAD);
    expect(TOK_LPAREN);
    
    std::string port = currentToken.value;
    expect(TOK_IDENTIFIER);
    expect(TOK_COMMA);
    
    std::string pin = currentToken.value;
    expect(TOK_NUMBER);
    
    output << "READ_PIN(" << port << ", " << pin << ")";
    
    expect(TOK_RPAREN);
}

void Parser::parseDelay() {
    expect(TOK_DELAY);
    expect(TOK_LPAREN);
    
    std::string delayMs = currentToken.value;
    expect(TOK_NUMBER);
    
    indent();
    output << "_delay_ms(" << delayMs << ");\n";
    
    expect(TOK_RPAREN);
}

void Parser::parseExpression() {
    parseTerm();
    
    while (currentToken.type == TOK_PLUS || currentToken.type == TOK_MINUS ||
           currentToken.type == TOK_EQ || currentToken.type == TOK_NE ||
           currentToken.type == TOK_LT || currentToken.type == TOK_LE ||
           currentToken.type == TOK_GT || currentToken.type == TOK_GE ||
           currentToken.type == TOK_AND || currentToken.type == TOK_OR) {
        
        TokenType op = currentToken.type;
        advance();
        
        if (op == TOK_PLUS) output << " + ";
        else if (op == TOK_MINUS) output << " - ";
        else if (op == TOK_EQ) output << " == ";
        else if (op == TOK_NE) output << " != ";
        else if (op == TOK_LT) output << " < ";
        else if (op == TOK_LE) output << " <= ";
        else if (op == TOK_GT) output << " > ";
        else if (op == TOK_GE) output << " >= ";
        else if (op == TOK_AND) output << " && ";
        else if (op == TOK_OR) output << " || ";
        
        parseTerm();
    }
}

void Parser::parseTerm() {
    parseFactor();
    
    while (currentToken.type == TOK_MULTIPLY || currentToken.type == TOK_DIVIDE) {
        TokenType op = currentToken.type;
        advance();
        
        output << (op == TOK_MULTIPLY ? " * " : " / ");
        parseFactor();
    }
}

void Parser::parseFactor() {
    if (currentToken.type == TOK_NUMBER) {
        output << currentToken.value;
        advance();
    } else if (currentToken.type == TOK_IDENTIFIER) {
        std::string name = currentToken.value;
        advance();
        
        // Check if it's a function call
        if (currentToken.type == TOK_LPAREN && subroutines.find(name) != subroutines.end()) {
            expect(TOK_LPAREN);
            output << name << "(";
            
            bool first = true;
            while (currentToken.type != TOK_RPAREN) {
                if (!first) {
                    output << ", ";
                    expect(TOK_COMMA);
                }
                first = false;
                parseExpression();
            }
            
            output << ")";
            expect(TOK_RPAREN);
        } else {
            output << name;
        }
    } else if (currentToken.type == TOK_TRUE) {
        output << "1";
        advance();
    } else if (currentToken.type == TOK_FALSE) {
        output << "0";
        advance();
    } else if (currentToken.type == TOK_HIGH) {
        output << "1";
        advance();
    } else if (currentToken.type == TOK_LOW) {
        output << "0";
        advance();
    } else if (currentToken.type == TOK_NOT) {
        advance();
        output << "!";
        parseFactor();
    } else if (currentToken.type == TOK_DIGITALREAD) {
        parseDigitalRead();
    } else if (currentToken.type == TOK_LPAREN) {
        advance();
        output << "(";
        parseExpression();
        output << ")";
        expect(TOK_RPAREN);
    } else if (currentToken.type == TOK_MINUS) {
        advance();
        output << "-";
        parseFactor();
    }
}

std::string Parser::compile() {
    parseProgram();
    return output.str();
}
