# f2avr
A simple fortran 90 compiler targeting avr-gcc, to effectivly produce binaries for Adriunos. 
F2AVR is the world’s first Fortran-to-C compiler designed to run on Arduino hardware.
The F2AVR compiler translates a subset of Fortran 90 into C code optimized for AVR microcontrollers. It focuses on embedded systems programming with direct hardware access.

## Table of Contents

- [Program Structure](#program-structure)
- [Data Types](#data-types)
- [Variables](#variables)
- [Operators](#operators)
- [Control Flow](#control-flow)
- [GPIO Functions](#gpio-functions)
- [Comments](#comments)
- [Examples](#examples)

---

## Compiling and uploading fortran code to a arduino
### Linux
*the f2avr compiler is depenedent on avr-gcc and avrdude to create binaries and upload them to the arduino; so make sure that they are installed before doing any of these*
- first either get the compiled version of the f2avr compiler from `releases` or follow these steps
  - clone the github repo `git clone https://www.github.com/speedskater1610/f2avr`
  - go into the repo `cd f2avr`
  - build the project into a exacutable `make`
- After you have the built exacutable the `arduino_upload.sh` script is very useful for building and uploading programs, to make this a a exacutable so you can run it do the following: `chmod +x arduino_upload.sh `.
- This should allow you to be able to run `./arduino_upload.sh tests/led_blink.f90` which if a arduino in plugged in the fortran program should upload.
- If you run into the problem of running the above command and get the following, you should run `sudo usermod -a -G dialout $USER` or on arch/manjaro `sudo usermod -aG uucp $USER` then log out and log back in, or reboot.
  ```sh
  OS error: cannot open port /dev/ttyUSB0: Permission denied
  Error: unable to open port /dev/ttyUSB0 for programmer arduino

  Avrdude done.  Thank you.
  [ERROR] Upload failed!
  ```


### Windows
- download the f2avr compiler form [releases](https://github.com/speedskater1610/f2avr/releases), *Make sure that it is the windows version*
- install the tools needed to run along side this project (`avrdude` & `avr-gcc`) the easiest way to install them is using msys2
  - if you done already have msys2 installed follow the following steps.
  - get msys2 from [here](msys2.org)
  - Run installer -> accept defaults
  - Open MSYS2 MSYS terminal (Not UCRT64 or MINGW64 yet)
  - update everything `pacman -Syu`
  - IMPORTANT: The terminal may tell you to close and reopen MSYS2. Do that, then run: `pacman -Syu` again until it says everything is up to date.
  - Install avr-gcc and avrdude `pacman -S avr-gcc avrdude`
  - now run `avr-gcc --version` and `avrdude --version`, you should get something telling you what version it is and not a error
  - *NOTE: instead of using the windows cmd or powershell use the `msys2 msys` shell; It could be useful to add this to your task-bar or homepage*
- Prepare your project by adding `f2avr.exe` and `arduino_upload.sh` in your project folder
- Make the script exacutable `chmod +x arduino_upload.sh`
- Then while inside of your project folder in `msys2 msys` shell go to your project folder, use `cd filePath/project_folder` to get there
- Next run `./arduino_upload.sh FORTRAN_FILE.f90` with your correct fortran file name, This should upload the code to your arduino.
- *NOTE: If you run into any issues please open up a issue on this github repo so I can either fix it or help you where you went wrong, please provide as much information as possible*
   
## Program Structure

Every Fortran program must follow this structure:

```fortran
PROGRAM program_name
  ! Variable declarations (optional)
  ! Statements
END
```

**Rules:**
- `PROGRAM` keyword starts the program
- Program name is required but can be any valid identifier
- `END` keyword terminates the program
- Generated C code includes AVR headers automatically

**Example:**
```fortran
PROGRAM hello
  INTEGER x
  x = 42
END
```

---

## Data Types

F2AVR supports three basic data types:

### INTEGER

Maps to `int16_t` in C (16-bit signed integer).

```fortran
INTEGER counter, limit, value
```

**Range:** -32,768 to 32,767

### REAL

Maps to `float` in C (32-bit floating point).

```fortran
REAL temperature, voltage
```

**Precision:** ~6-7 decimal digits

### LOGICAL

Maps to `uint8_t` in C (8-bit unsigned integer).

```fortran
LOGICAL flag, is_ready
```

**Values:** `.TRUE.` (1) or `.FALSE.` (0)

---

## Variables

### Declaration

Variables must be declared before use:

```fortran
INTEGER x, y, z
REAL pi, e
LOGICAL done
```

**Rules:**
- Declare at the beginning of the program (after `PROGRAM`)
- Multiple variables of the same type can be comma-separated
- All variables are initialized to 0
- Variable names are case-insensitive (converted to uppercase internally)

### Assignment

```fortran
x = 10
y = x + 5
temperature = 25.5
done = .TRUE.
```

---

## Operators

### Arithmetic Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `+` | Addition | `x = 5 + 3` |
| `-` | Subtraction | `x = 10 - 4` |
| `*` | Multiplication | `x = 6 * 7` |
| `/` | Division | `x = 20 / 4` |
| `-` (unary) | Negation | `x = -5` |

**Example:**
```fortran
result = (a + b) * c - d / 2
```

### Relational Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `==` | Equal to | `IF (x == 10)` |
| `/=` | Not equal to | `IF (x /= 0)` |
| `<` | Less than | `IF (x < 100)` |
| `<=` | Less than or equal | `IF (x <= 50)` |
| `>` | Greater than | `IF (x > 0)` |
| `>=` | Greater than or equal | `IF (x >= 10)` |

### Logical Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `.AND.` | Logical AND | `IF (x > 0 .AND. x < 100)` |
| `.OR.` | Logical OR | `IF (x == 0 .OR. y == 0)` |
| `.NOT.` | Logical NOT | `IF (.NOT. done)` |

**Constants:**
- `.TRUE.` - Boolean true (compiles to 1)
- `.FALSE.` - Boolean false (compiles to 0)

---

## Control Flow

### IF Statement

```fortran
IF (condition) THEN
  ! statements
ENDIF
```

**With ELSE:**
```fortran
IF (condition) THEN
  ! statements when true
ELSE
  ! statements when false
ENDIF
```

**Example:**
```fortran
IF (temperature > 30) THEN
  CALL DIGITALWRITE(B, 5, HIGH)  ! Turn on fan
ELSE
  CALL DIGITALWRITE(B, 5, LOW)   ! Turn off fan
ENDIF
```

**Rules:**
- Condition must be in parentheses
- `THEN` is required
- `ENDIF` closes the block
- Nested IF statements are supported

### DO WHILE Loop

```fortran
DO WHILE (condition)
  ! statements
ENDDO
```

**Example:**
```fortran
counter = 0
DO WHILE (counter < 10)
  counter = counter + 1
ENDDO
```

**Infinite loop:**
```fortran
DO WHILE (1 == 1)  ! Or any always-true condition
  ! runs forever
ENDDO
```

**Rules:**
- Condition must be in parentheses
- `ENDDO` closes the loop
- No `DO...UNTIL` or counted `DO` loops (use `DO WHILE` with counter)
- Nested loops are supported

---

## GPIO Functions

F2AVR provides Arduino-style functions for GPIO control.

### PINMODE

Configure a pin as input or output.

```fortran
CALL PINMODE(port, pin, mode)
```

**Parameters:**
- `port` - Port letter: `B`, `C`, or `D`
- `pin` - Pin number: 0-7
- `mode` - `INPUT` or `OUTPUT`

**Example:**
```fortran
CALL PINMODE(B, 5, OUTPUT)  ! PB5 as output (Arduino pin 13)
CALL PINMODE(D, 2, INPUT)   ! PD2 as input (Arduino pin 2)
```

**AVR Port Mapping (Arduino Uno):**
- Port B: Digital pins 8-13
- Port C: Analog pins A0-A5
- Port D: Digital pins 0-7

### DIGITALWRITE

Set a pin HIGH or LOW.

```fortran
CALL DIGITALWRITE(port, pin, state)
```

**Parameters:**
- `port` - Port letter: `B`, `C`, or `D`
- `pin` - Pin number: 0-7
- `state` - `HIGH` (1) or `LOW` (0), or a variable

**Example:**
```fortran
CALL DIGITALWRITE(B, 5, HIGH)  ! Turn on LED
CALL DIGITALWRITE(B, 5, LOW)   ! Turn off LED

! Using a variable
INTEGER led_state
led_state = HIGH
CALL DIGITALWRITE(B, 5, led_state)
```

### DIGITALREAD

Read the state of a pin.

```fortran
variable = DIGITALREAD(port, pin)
```

**Parameters:**
- `port` - Port letter: `B`, `C`, or `D`
- `pin` - Pin number: 0-7

**Returns:** 0 (LOW) or 1 (HIGH)

**Example:**
```fortran
INTEGER button_state
button_state = DIGITALREAD(D, 2)

IF (button_state == HIGH) THEN
  ! Button is pressed
ENDIF
```

### DELAY

Pause execution for specified milliseconds.

```fortran
CALL DELAY(milliseconds)
```

**Parameters:**
- `milliseconds` - Delay duration (integer constant)

**Example:**
```fortran
CALL DELAY(1000)  ! Wait 1 second
CALL DELAY(500)   ! Wait 0.5 seconds
```

**Note:** Uses `_delay_ms()` from AVR libc, which is a compile-time constant delay.

---

## Comments

Use `!` for single-line comments:

```fortran
! This is a comment
INTEGER x  ! Inline comment
```

**Rules:**
- Comments start with `!` and continue to end of line
- Can appear anywhere on a line
- Not preserved in generated C code

---

### Functions and subroutines
Use the following syntax to declare a Functions or Subroutine
```fortran
INTEGER FUNCTION read_average(pin, samples)
  INTEGER sum, i
  sum = 0
  i = 0
  
  DO WHILE (i < samples)
    sum = sum + DIGITALREAD(D, pin)
    CALL DELAY(10)
    i = i + 1
  ENDDO
  
  read_average = sum / samples
END
```
Use the following syntax to call a Function or Subroutine
```fortran
PROGRAM smart_button
  INTEGER avg
  
  CALL PINMODE(D, 2, INPUT)
  CALL PINMODE(B, 5, OUTPUT)
  
  DO WHILE (1 == 1)
    avg = read_average(2, 5)
    
    IF (avg > 0) THEN
      CALL DIGITALWRITE(B, 5, HIGH)
    ELSE
      CALL DIGITALWRITE(B, 5, LOW)
    ENDIF
    
    CALL DELAY(100)
  ENDDO
END
```

### Goto statements
use the following program as a reference on how to use goto
```fortran
PROGRAM traffic_light
  CALL PINMODE(B, 5, OUTPUT)  ! Red
  CALL PINMODE(B, 4, OUTPUT)  ! Yellow
  CALL PINMODE(B, 3, OUTPUT)  ! Green

start:
  ! Red
  CALL DIGITALWRITE(B, 5, HIGH)
  CALL DIGITALWRITE(B, 4, LOW)
  CALL DIGITALWRITE(B, 3, LOW)
  CALL DELAY(3000)

  ! Yellow
  CALL DIGITALWRITE(B, 5, LOW)
  CALL DIGITALWRITE(B, 4, HIGH)
  CALL DIGITALWRITE(B, 3, LOW)
  CALL DELAY(1000)
  ! Green
  CALL DIGITALWRITE(B, 5, LOW)
  CALL DIGITALWRITE(B, 4, LOW)
  CALL DIGITALWRITE(B, 3, HIGH)
  CALL DELAY(3000)
  
  GOTO start  ! Loop forever
END
```

### Syntax Requirements

- Case insensitive (all converted to uppercase)
- Line-based parsing (no line continuations with `&`)
- Parentheses required for conditions: `IF (x > 0)` not `IF x > 0`
- `THEN` required after IF condition
- All blocks must be explicitly closed (`ENDIF`, `ENDDO`)

### AVR-Specific Constraints

- No standard I/O (PRINT statements become comments)
- Limited memory (optimize variable usage)
- Integer division truncates (no automatic promotion)
- Floating point is slow on AVR (use integers when possible)

---

## Examples

### LED Blink

```fortran
PROGRAM blink
  CALL PINMODE(B, 5, OUTPUT)
  
  DO WHILE (1 == 1)
    CALL DIGITALWRITE(B, 5, HIGH)
    CALL DELAY(1000)
    CALL DIGITALWRITE(B, 5, LOW)
    CALL DELAY(1000)
  ENDDO
END
```

### Button Input

```fortran
PROGRAM button
  INTEGER state
  
  CALL PINMODE(D, 2, INPUT)
  CALL PINMODE(B, 5, OUTPUT)
  
  DO WHILE (1 == 1)
    state = DIGITALREAD(D, 2)
    CALL DIGITALWRITE(B, 5, state)
    CALL DELAY(10)
  ENDDO
END
```

### PWM Simulation (Soft)

```fortran
PROGRAM soft_pwm
  INTEGER duty, i
  
  CALL PINMODE(B, 5, OUTPUT)
  duty = 25  ! 25% duty cycle
  
  DO WHILE (1 == 1)
    i = 0
    DO WHILE (i < 100)
      IF (i < duty) THEN
        CALL DIGITALWRITE(B, 5, HIGH)
      ELSE
        CALL DIGITALWRITE(B, 5, LOW)
      ENDIF
      CALL DELAY(1)
      i = i + 1
    ENDDO
  ENDDO
END
```

### State Machine

```fortran
PROGRAM state_machine
  INTEGER state, button
  
  CALL PINMODE(D, 2, INPUT)
  CALL PINMODE(B, 5, OUTPUT)
  CALL PINMODE(B, 4, OUTPUT)
  
  state = 0
  
  DO WHILE (1 == 1)
    button = DIGITALREAD(D, 2)
    
    IF (button == HIGH) THEN
      IF (state == 0) THEN
        state = 1
      ELSE
        state = 0
      ENDIF
      CALL DELAY(200)  ! Debounce
    ENDIF
    
    IF (state == 0) THEN
      CALL DIGITALWRITE(B, 5, LOW)
      CALL DIGITALWRITE(B, 4, HIGH)
    ELSE
      CALL DIGITALWRITE(B, 5, HIGH)
      CALL DIGITALWRITE(B, 4, LOW)
    ENDIF
    
    CALL DELAY(10)
  ENDDO
END
```

### Counter with Overflow

```fortran
PROGRAM counter
  INTEGER count
  
  CALL PINMODE(B, 5, OUTPUT)
  count = 0
  
  DO WHILE (1 == 1)
    IF (count >= 10) THEN
      count = 0
    ENDIF
    
    ! Blink count times
    INTEGER i
    i = 0
    DO WHILE (i < count)
      CALL DIGITALWRITE(B, 5, HIGH)
      CALL DELAY(100)
      CALL DIGITALWRITE(B, 5, LOW)
      CALL DELAY(100)
      i = i + 1
    ENDDO
    
    count = count + 1
    CALL DELAY(1000)
  ENDDO
END
```

---

## Generated C Code

F2AVR generates clean, readable C code with:

- `#include <avr/io.h>` - AVR I/O definitions
- `#include <util/delay.h>` - Delay functions
- `#include <stdint.h>` - Fixed-width integer types
- GPIO macros for direct port manipulation
- `main()` function with infinite loop support

**Example Generated Code:**

```c
// Generated from Fortran program: BLINK
#include <avr/io.h>
#include <util/delay.h>
#include <stdint.h>

// GPIO helper macros
#define SET_OUTPUT(port, pin) DDR##port |= (1 << pin)
#define SET_INPUT(port, pin) DDR##port &= ~(1 << pin)
#define SET_HIGH(port, pin) PORT##port |= (1 << pin)
#define SET_LOW(port, pin) PORT##port &= ~(1 << pin)
#define READ_PIN(port, pin) ((PIN##port >> pin) & 1)

int main(void) {
  SET_OUTPUT(B, 5);

  while (1 == 1) {
    SET_HIGH(B, 5);
    _delay_ms(1000);
    SET_LOW(B, 5);
    _delay_ms(1000);
  }

  return 0;
}
```

---

## Error Handling

The compiler provides basic error messages:

- **Parse errors**: Show line number where error occurred
- **Unknown characters**: Reports unexpected tokens
- **File I/O errors**: Reports if input/output files cannot be opened

**Tips for debugging:**
1. Check your Fortran syntax matches the supported subset
2. Ensure all blocks are properly closed (ENDIF, ENDDO)
3. Verify variable declarations come before statements
4. Look at generated C code to understand translation

---

## Tips for Best Results

### Performance

- **Use integers**: Much faster than floating point on AVR
- **Avoid division**: Use bit shifts for powers of 2
- **Minimize variables**: Limited RAM on AVR
- **Inline calculations**: Reduces stack usage
- **goto instead of infinate while loops**: while loops when a condition before running each time.

### Code Organization

- **One task per program**: Keep programs focused
- **Use meaningful names**: `led_pin` not `x`
- **Comment your intent**: Explain why, not what
- **Test incrementally**: Start simple, add complexity

### Hardware Considerations

- **Set pin modes**: Always call PINMODE before using pins
- **Debounce inputs**: Add delays after reading buttons
- **Limit delay use**: Blocks entire program
- **Pull-up resistors**: Consider INPUT_PULLUP (not yet supported, wire externally)

---

## Future Enhancements

Possible future additions to the compiler:

- One-dimensional arrays
- FOR loops with automatic counting
- More GPIO functions (analogRead, analogWrite)
- PARAMETER for constants
- Simple subroutines
- Bit manipulation functions
- Interrupt support
- Serial communication functions

---

## Compiler Information

**Version:** 0.1.0  
**Language Standard:** Fortran 90 (subset)  
**Target:** AVR microcontrollers  
**Output:** ANSI C (avr-gcc)  
**License:** GPL v.3
