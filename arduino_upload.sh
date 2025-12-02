#!/bin/bash

# Arduino Upload Script
set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Logging (stderr only)
print_info() { echo -e "${BLUE}[INFO]${NC} $1" >&2; }
print_success() { echo -e "${GREEN}[SUCCESS]${NC} $1" >&2; }
print_warning() { echo -e "${YELLOW}[WARNING]${NC} $1" >&2; }
print_error() { echo -e "${RED}[ERROR]${NC} $1" >&2; }

# Detect Arduino
detect_arduino() {
    print_info "Searching for Arduino boards..."

    local arduino_ports=()
    for port in /dev/ttyUSB* /dev/ttyACM*; do
        [ -e "$port" ] && arduino_ports+=("$port")
    done

    if [ ${#arduino_ports[@]} -eq 0 ]; then
        print_error "No Arduino found!"
        exit 1
    elif [ ${#arduino_ports[@]} -eq 1 ]; then
        ARDUINO_PORT="${arduino_ports[0]}"
        print_success "Found Arduino at: $ARDUINO_PORT"
    else
        print_warning "Multiple devices found:"
        for i in "${!arduino_ports[@]}"; do
            echo "  [$i] ${arduino_ports[$i]}" >&2
        done
        read -p "Select device number [0]: " choice
        choice=${choice:-0}
        ARDUINO_PORT="${arduino_ports[$choice]}"
        print_info "Using: $ARDUINO_PORT"
    fi
}

# Detect MCU type
detect_board_type() {
    print_info "Detecting board type..."

    if command -v lsusb &>/dev/null; then
        local usb=$(lsusb | grep -i "arduino\|ch340\|ftdi\|mega\|uno" || true)

        if echo "$usb" | grep -qi "328"; then
            MCU="atmega328p"
            PROGRAMMER="arduino"
            BAUD="115200"
            F_CPU="16000000UL"
            print_success "Detected: Arduino Uno (ATmega328P)"
        else
            print_warning "Could not auto-detect board type"
            MCU="atmega328p"
            PROGRAMMER="arduino"
            BAUD="115200"
            F_CPU="16000000UL"
        fi
    fi
}

# Compile Fortran → C
compile_fortran() {
    local f90_file="$1"
    local c_file="${f90_file%.f90}.c"

    print_info "Compiling Fortran to C..."

    local f2avr_path="./f2avr"
    if ! [ -f "$f2avr_path" ]; then
        print_error "f2avr not found!"
        exit 1
    fi

    print_info "Using compiler: $f2avr_path"

    if ! "$f2avr_path" "$f90_file" "$c_file" >&2; then
        print_error "Fortran compilation failed!"
        exit 1
    fi

    print_success "Generated: $c_file"
    echo "$c_file" # stdout → caller
}

# Compile C → ELF → HEX
compile_to_hex() {
    local c_file="$1"
    local base="${c_file%.c}"
    local elf="${base}.elf"
    local hex="${base}.hex"

    print_info "Compiling C to machine code..."

    if ! command -v avr-gcc &>/dev/null; then
        print_error "avr-gcc not installed"
        exit 1
    fi

    print_info "Running avr-gcc..."
    if ! avr-gcc -mmcu="$MCU" -DF_CPU="$F_CPU" -O2 -Wall "$c_file" -o "$elf" >&2; then
        print_error "C compilation failed!"
        exit 1
    fi

    print_success "Compiled: $elf"

    print_info "Generating HEX..."
    if ! avr-objcopy -O ihex -R .eeprom "$elf" "$hex" >&2; then
        print_error "Hex generation failed!"
        exit 1
    fi

    print_success "Generated: $hex"
    echo "$hex"
}

# Upload
upload_to_arduino() {
    local hex="$1"

    print_info "Uploading to Arduino..."

    if ! command -v avrdude &>/dev/null; then
        print_error "avrdude not installed"
        exit 1
    fi

    if ! avrdude -p "$MCU" -c "$PROGRAMMER" -P "$ARDUINO_PORT" -b "$BAUD" -D -U flash:w:"$hex":i; then
        print_error "Upload failed!"
        exit 1
    fi

    print_success "Upload complete!"
}

### MAIN ###
echo -e "${BLUE}================================${NC}" >&2
echo -e "${BLUE}  Arduino Fortran Uploader${NC}" >&2
echo -e "${BLUE}================================${NC}" >&2
echo "" >&2

if [ $# -lt 1 ]; then
    print_error "Usage: $0 file.f90|file.c|file.hex"
    exit 1
fi

INPUT_FILE="$1"
shift

if [ ! -f "$INPUT_FILE" ]; then
    print_error "File does not exist: $INPUT_FILE"
    exit 1
fi

detect_arduino
detect_board_type

ext="${INPUT_FILE##*.}"

case "$ext" in
f90)
    print_info "Input: Fortran source"
    c_file=$(compile_fortran "$INPUT_FILE")
    hex=$(compile_to_hex "$c_file")
    ;;
c)
    print_info "Input: C source"
    hex=$(compile_to_hex "$INPUT_FILE")
    ;;
hex)
    print_info "Input: HEX ready to upload"
    hex="$INPUT_FILE"
    ;;
*)
    print_error "Unsupported file type: .$ext"
    exit 1
    ;;
esac

upload_to_arduino "$hex"
print_success "All done!"
