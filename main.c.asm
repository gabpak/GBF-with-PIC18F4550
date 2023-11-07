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
REGA	equ 0x01
REGB	equ 0x02
REGC	equ 0x03
		
DIVISION_NUMERATOR	equ 0x0A ; The numerator of the division
DIVISION_DENOMINATOR	equ 0x0B ; The denominator of the division
DIVISION_RESULT		equ 0x0C ; The result of the division
DIVISION_MODULO		equ 0x0D ; The rest of the euclidian division
		
NUMBER_7_SEGMENTS	equ 0x10 ; The number to display on the 7-segments
		
		
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
    CLRF REGA
    CLRF REGB
    CLRF REGC
    
    CLRF DIVISION_NUMERATOR
    CLRF DIVISION_DENOMINATOR
    CLRF DIVISION_RESULT
    CLRF DIVISION_MODULO
  

    GOTO MAIN

    ; ______________________________________________________________________
    
    
IRQ_HANDLE
    BTFSC PIR1, ADIF ; ADIF interrupt flag for A/D Converter
    GOTO AD_CONVERSION ; Yes
    RETFIE ; No
 
AD_CONVERSION
    BCF PIR1, ADIF ; Init back ADIF to 0x00
    MOVFF ADRESH, PORTB ; Start the conversion here ..
    MOVF ADRESH, W ; Put the result inside the W for the display
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
_DISPLAY
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
    
DISPLAY
    ; 987 % 10 = 7
    ; DISPLAY 7
    ; RA0
    
    ; Result = (987 - 7) / 10 = 98
    ; 98 % 10 = 8
    ; DISPLAY 8
    ; RA1
    
    ; Result = (98 - 8) % 10 = 9
    ; 9 % 10 = 9
    ; DISPLAY 9
    ; RA2
    
    ; Result = (9 - 9) / 10
    ; 0 % 10 = 0
    ; DISPLAY 0
    ; RA3
    
    
    RETURN

DIVISION
    CLRF DIVISION_RESULT ; Init Result
    MOVFF DIVISION_NUMERATOR, REGA ; REGA will remplace DIVISION_NUMERATOR because we need it after
    ; Check here the 0 division ...
    ; Check here the Num > Den
    
    DIVISION_LOOP
    MOVF DIVISION_DENOMINATOR, WREG ; Loading DIVISION_DENOMINATOR inside the WREG
    SUBWF REGA, W ; WREG - REGA => WREG
    BTFSC STATUS, N ; Negatif ? Skip if Clear
    RETURN ; Yes
    INCF DIVISION_RESULT, F ; No
    MOVWF REGA ; WREG => REGA for the new calculus in the next iteration
    GOTO DIVISION_LOOP
    
MODULO
    ; REGA = DIVISION_RESULT
    ; MODULO_LOOP
    ; If REGA = 0
	; MODULO_RESULT = DIVISION_NUMERATEUR
	; GOTO CALCUL_REST 
    ; REGB += DIVISION_DENOMINATEUR
    ; REGA -- 
    ; GOTO MODULO_LOOP
    ; CALCUL_REST
    ; MODULO_RESULT = DIVISION_NUMERATEUR - REGB
    ; RETURN
    
    MOVFF DIVISION_RESULT, REGA ; We move DIVISION_RESULT to REGA
    MODULO_LOOP
    MOVLW 0x00
    ;MOVWF REGA ; Just for test
    CPFSEQ REGA ; Compare REGA with 0x00
    GOTO NOT_ZERO; Condition is Not 0
    MOVFF DIVISION_NUMERATOR, DIVISION_MODULO ; Condition is 0
    RETURN
    NOT_ZERO
    MOVFF DIVISION_DENOMINATOR, WREG
    ADDWF REGB ;  REGB++ => REGB
    DECF REGA
    MOVLW 0x00
    CPFSEQ REGA ; Compare with 0x00
    GOTO NOT_ZERO; Not 0
    ; Now we make the difference between the difference
    MOVFF REGB, WREG ; We place REGB Accu inside WREG => W
    MOVFF DIVISION_NUMERATOR, REGA ; DIVISION_NUMERATOR => REGA
    SUBWF REGA ; REGA - W = DIVISION_NUMERATOR - REGB => REGA
    MOVFF REGA, DIVISION_MODULO
    RETURN

MAIN
    MOVLW 0x0A ; 47
    MOVWF DIVISION_NUMERATOR
    MOVLW 0x0A ; 10
    MOVWF DIVISION_DENOMINATOR
    CALL DIVISION ; 23 / 10 => DIVISION_RESULT
    CALL MODULO ; 3 => MODULO_RESULT
    GOTO MAIN
    END