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
    CLRF TRISA ; Define port A as an Output for the 7-segments/transistor
    
    ; **** A/D Module ****
    MOVLW b'00010101' ; RA5 Port .3
    
    MOVWF ADCON0
    
    GOTO MAIN

; High Priority Interrupt Handle
IRQ_HANDLE
    RETFIE

; Low Priority Interrupt Handle
AD_RESULT
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
    CALL DISPLAY
    GOTO MAIN

END