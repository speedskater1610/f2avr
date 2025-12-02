// Generated from Fortran program: BUTTON_LED
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
  int16_t BUTTON_STATE = 0;

  SET_INPUT(D, 2);
  SET_OUTPUT(B, 5);
  while (1 == 1) {
    BUTTON_STATE = READ_PIN(D, 2);
    if (BUTTON_STATE == 1) {
      SET_HIGH(B, 5);
    } else {
      SET_LOW(B, 5);
    }
    _delay_ms(50);
  }

return 0;
}
