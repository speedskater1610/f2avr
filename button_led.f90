PROGRAM button_led
  INTEGER button_state
  
  ! Configure pins
  CALL PINMODE(D, 2, INPUT)   ! Button on PD2
  CALL PINMODE(B, 5, OUTPUT)  ! LED on PB5
  
  DO WHILE (1 == 1)
    button_state = DIGITALREAD(D, 2)
    
    IF (button_state == HIGH) THEN
      CALL DIGITALWRITE(B, 5, HIGH)
    ELSE
      CALL DIGITALWRITE(B, 5, LOW)
    ENDIF
    
    CALL DELAY(50)
  ENDDO
END
