# Compiler settings
CXX = g++
CXXFLAGS = -std=c++11 -Wall -Wextra -O2
TARGET = f2avr

# Source files
SRCS = src/main.cpp src/lexer.cpp src/parser.cpp
OBJS = $(SRCS:.cpp=.o)

# Header files for dependency tracking
HEADERS = token.h lexer.h parser.h

# Default target
all: $(TARGET)

# Link the executable
$(TARGET): $(OBJS)
	$(CXX) $(CXXFLAGS) -o $(TARGET) $(OBJS)

# Compile source files
%.o: %.cpp $(HEADERS)
	$(CXX) $(CXXFLAGS) -c $< -o $@

# Clean build artifacts
clean:
	rm -f $(OBJS) $(TARGET)

# Install to /usr/local/bin (requires sudo)
install: $(TARGET)
	install -m 0755 $(TARGET) /usr/local/bin

# Uninstall
uninstall:
	rm -f /usr/local/bin/$(TARGET)

# Test with example
test: $(TARGET)
	@echo "PROGRAM test\n  INTEGER x\n  x = 42\nEND" > test.f90
	./$(TARGET) test.f90 test.c
	@echo "Generated test.c:"
	@cat test.c
	@rm -f test.f90 test.c

# Upload script
upload: $(TARGET)
	@if [ ! -f "arduino_upload.sh" ]; then \
		echo "Error: arduino_upload.sh not found"; \
		exit 1; \
	fi
	@chmod +x arduino_upload.sh
	@echo "Upload script ready. Usage: ./arduino_upload.sh <file.f90|.c|.hex>"

# Install upload script
install-upload: upload
	install -m 0755 arduino_upload.sh /usr/local/bin/f2avr-upload
	@echo "Installed as: f2avr-upload"
	@echo "Usage: f2avr-upload <file.f90>"

# Quick compile and upload
flash: $(TARGET)
	@if [ -z "$(FILE)" ]; then \
		echo "Usage: make flash FILE=yourfile.f90"; \
		exit 1; \
	fi
	@chmod +x arduino_upload.sh
	./arduino_upload.sh $(FILE)

# Phony targets
.PHONY: all clean install uninstall test upload install-upload flash

