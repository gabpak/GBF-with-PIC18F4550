#include <p18F4550.inc>

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
    
ORG 0x0000
    GOTO INIT
ORG 0x0008
    GOTO IRQ_HANDLE

INIT
    MOVLW b'00100000' ; On active que le Port 5 du port A en Input, le reste est en output pour le 7 segment.
    MOVWF TRISA
    CLRF PORTA ; Cleaning the 7-segments/transistors port A
    
    CLRF PORTC ; Affichage du resultat A/D sur le port C
    CLRF TRISC
    
    CLRF PORTD
    CLRF TRISD

    ; Configuration of the AD Interrupts / ADCON0
    MOVLW b'00010001'
    MOVWF ADCON0
    
    ; (Page 267 / 271)
    ; Enable AD Interrupts / ADCON1
    MOVLW B'00001010'
    MOVWF ADCON1

    ; Left side bit
    MOVLW b'00000000'
    MOVWF ADCON2
    
    ; Intterupts Enables
    BSF PIE1, ADIE
    BSF INTCON, PEIE
    BSF INTCON, GIE
    
    ; ADIF interrupt flag
    GOTO MAIN
    
IRQ_HANDLE
    BTFSC PIR1, ADIF
    GOTO AD_CONVERSION ; Yes
    RETFIE ; No
    
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

AD_CONVERSION
    BCF PIR1, ADIF ; Init
    ; Start the conversion here ..
    MOVFF ADRESH, PORTC
    RETFIE
    
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