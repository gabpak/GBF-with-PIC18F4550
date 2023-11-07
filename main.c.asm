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
	
; MEMORY EMPLACEMENT
TEMP		equ 0x01
		
DIVISION_NUMERATOR	equ 0x02 ; The numerator of the division
DIVISION_DENOMINATOR	equ 0x03 ; The denominator of the division
DIVISION_RESULT		equ 0x04 ; The result of the division
DIVISION_MODULO		equ 0x05 ; The rest of the euclidian division
		
		
ORG 0x0000
    GOTO INIT
ORG 0x0008
    GOTO IRQ_HANDLE

INIT
    MOVLW b'00100000' ; On active que le Port 5 du port A en Input, le reste est en output pour le 7 segment.
    MOVWF TRISA
    CLRF PORTA ; Cleaning the 7-segments/transistors port A

    CLRF PORTB ; Affichage du resultat A/D sur le port C
    CLRF TRISB
    
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
    
    ; RESET GLOBAL VARIABLES
    CLRF TEMP
    CLRF DIVISION_NUMERATOR
    CLRF DIVISION_DENOMINATOR
    CLRF DIVISION_RESULT
  

    GOTO MAIN

    
    
    ; ______________________________________________________________________
    
    
IRQ_HANDLE
    BTFSC PIR1, ADIF ; ADIF interrupt flag for A/D Converter
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
    BCF PIR1, ADIF ; Init back ADIF to 0x00
    MOVFF ADRESH, PORTB ; Start the conversion here ..
    MOVF ADRESH, W ; Put the result inside the W for the display
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

    MOVLW VARLCD9
    MOVWF PORTD
    BSF PORTA, RA0

    CALL DELAY
    RETURN

DIVISION
    CLRF DIVISION_RESULT ; Init Result
    MOVFF DIVISION_NUMERATOR, TEMP ; TEMP will remplace DIVISION_NUMERATOR because we need it after
    LOOP
    ;MOVLW 0x0A
    MOVF DIVISION_DENOMINATOR, WREG ; Loading DIVISION_DENOMINATOR inside the WREG
    SUBWF TEMP, W ; WREG - TEMP => WREG
    BTFSC STATUS, N ; Negatif ?
    RETURN
    INCF DIVISION_RESULT, F ; No
    MOVWF TEMP ; WREG => TEMP for the new calculus in the next iteration
    GOTO LOOP

MAIN
    MOVLW 0x2F ; 23
    MOVWF DIVISION_NUMERATOR
    MOVLW 0x07 ; 10
    MOVWF DIVISION_DENOMINATOR
    CALL DIVISION ; 23 / 10 => WREG
    GOTO MAIN
    END