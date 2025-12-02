#include "parser.h"
#include <iostream>
#include <fstream>
#include <sstream>

int main(int argc, char* argv[]) {
    // read through command line to get the file
    if (argc != 3) {
        std::cerr << "Usage: " << argv[0] << " <input.f90> <output.c>\n";
        return 1;
    }
    
    std::ifstream inputFile(argv[1]);
    if (!inputFile) {
        std::cerr << "Error: Cannot open input file " << argv[1] << "\n";
        return 1;
    }
    
    std::stringstream buffer;
    buffer << inputFile.rdbuf();
    std::string source = buffer.str();
    
    Parser parser(source);
    std::string cCode = parser.compile();
    
    std::ofstream outputFile(argv[2]);
    if (!outputFile) {
        std::cerr << "Error: Cannot create output file " << argv[2] << "\n";
        return 1;
    }
    
    outputFile << cCode;
    
    std::cout << "Compilation successful!\n";
    std::cout << "Generated C code written to " << argv[2] << "\n";
    
    return 0;
}
