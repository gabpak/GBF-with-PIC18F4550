#include "p18f4550.inc"

;Constant variable
COUNT	equ b'0000111'
VARLCD0	equ b'0111111'
VARLCD1	equ b'0000110'
VARLCD2	equ b'1011011'
VARLCD3	equ b'1001111'
VARLCD4 equ b'1100110'
VARLCD5	equ b'1101101'
VARLCD6	equ b'1111101'
VARLCD7	equ b'0000111'
VARLCD8	equ b'1111111'
VARLCD9	equ b'1101111'
	
; Setting the Start and Interrupts
ORG 0x0000
    GOTO INIT ; Init the PIC at the start of the program
ORG 0x0008
    GOTO IRQ_HANDLE ; High Priority Interrupts
ORG 0x0018
    GOTO MAIN ; Low Priority Interrupts 
    
; Init the PIC18F4550 Board
INIT
    ; **** 7 Segments ****
    CLRF PORTD ; Cleaning the 7 Segments port D
    CLRF TRISD ; Define port D as an Output for the 7 Segments
    CLRF PORTA ; Cleaning the 7-segments/transistors port A
    CLRF TRISA ; Define port A as an Output for the 7-segments/transistor AND for AN5 for the lecture of A/D Convertor
    
    ; **** A/D Module ****
    MOVLW b'00010001' ; (RA5/AN4) as the input for the AD Convertor + ADON
    MOVWF ADCON0
    
    MOVLW b'00001001' ; AN0 to AN5 are Analog
    MOVWF ADCON1
    
    MOVLW b'10000000'
    MOVWF ADCON2
    
    ; Interrupts of A/D
    BCF PIR1, ADIF ; Clear the interrupt bit
    BSF PIE1, ADIE ; Enable the A/D interrupt 
    BSF INTCON, GIE/GIEH ; Global Interrupt Enable bit
    
    BSF TRISA, TRISA5 ; A5 as an Input for the AD interrupt
    
    CLRF PORTB ; Clearing port B
    CLRF TRISB ; Setting port B as the output of the A/D Result
    
    GOTO MAIN

; High Priority Interrupt Handle
IRQ_HANDLE
    BTFSC PIR1, ADIF ; A/D Interrupt is completed ?
    GOTO AD_RESULT ; Yes
    RETFIE ; No

; Low Priority Interrupt Handle
AD_RESULT
    BCF PIR1, ADIF ; Clearing the interrupt bit
    MOVFF ADRESH, PORTB ; Putting the result on the port B (LEDS)
    RETFIE

; Delay function that use the global variable COUNT
DELAY
    DECFSZ COUNT
    GOTO DELAY
    CALL RESET_COUNT_DELAY
    RETURN
; Reset the global COUNT variable (used in delay function)
RESET_COUNT_DELAY
    SETF COUNT, b'00000111'
    RETURN

; Draw on the 7 segments
DISPLAY
    MOVLW VARLCD1
    MOVWF PORTD
    BSF PORTA, RA3
    
    CALL DELAY
    
    MOVLW VARLCD9
    MOVWF PORTD
    BSF PORTA, RA2
    
    CALL DELAY

    MOVLW VARLCD9
    MOVWF PORTD
    BSF PORTA, RA1
    
    CALL DELAY
    
    MOVLW VARLCD7
    MOVWF PORTD
    BSF PORTA, RA0
    
    CALL DELAY
    RETURN
    
MAIN
    BSF ADCON0, GO/DONE
    CALL DISPLAY
    GOTO MAIN

END