#include <p18F4550.inc>

;Constant variable
COUNT_TIMER equ 0x0A
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
	
COUNT	equ 0x08
		
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

    CLRF TRISB
    CLRF PORTB ; Affichage du resultat A/D sur le port C

    CLRF TRISD ; Utilis? pour l'affichage du 7 segment
    CLRF PORTD  
    
    CLRF TRISE ; Utilis? pour le signal rectangle
    CLRF PORTE

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
    
    MOVLW COUNT_TIMER
    MOVWF COUNT
  
    GOTO INIT_PWM

INIT_PWM
    ; C as an output
    BCF TRISC, TRISC2
    CLRF PORTC
    
    ; Interrupts
    BSF PIE1, TMR2IE
    BSF IPR1, TMR2IP ; High Priority
    
    MOVLW 0x7C
    MOVWF PR2 ; Period
    MOVLW b'00101000'
    MOVWF CCPR1L ; Duty cycle
    
    MOVLW b'00000010'
    MOVWF T2CON ; Prescale ? 16
    MOVLW b'00101100'
    MOVWF CCP1CON ; PWM mode and PWM Duty cycle 2 LSB at 10
    
    BSF T2CON, TMR2ON
    
    GOTO MAIN
    ; ______________________________________________________________________
    
    
IRQ_HANDLE
    BTFSC PIR1, ADIF ; ADIF interrupt flag for A/D Converter
    GOTO AD_CONVERSION ; Yes
    BTFSC PIR1, TMR2IF ; PWM Intterupt
    GOTO TMR2_IF ; Yes
    RETFIE ; No
 
AD_CONVERSION
    BCF PIR1, ADIF ; Init back ADIF to 0x00
    MOVFF ADRESH, PORTB ; Start the conversion here ..
    RETFIE
    
TMR2_IF ; Flag TMR2IF Raised
    BCF PIR1, TMR2IF
    RETFIE

    ; Delay function that use the global variable COUNT
DELAY
    DECFSZ COUNT
    GOTO DELAY
    CALL RESET_COUNT_DELAY
    RETURN
; Reset the global COUNT variable (used in delay function)
RESET_COUNT_DELAY
    MOVLW COUNT_TIMER
    MOVWF COUNT
    RETURN
    
DISPLAY_DECODER
    ; ZERO
    MOVLW 0x00
    CPFSEQ DIVISION_MODULO ; SKIP if f = W
    GOTO ONE; NO
    MOVLW VARLCD0 ; YES
    RETURN
    ONE
    MOVLW 0x01
    CPFSEQ DIVISION_MODULO ; SKIP if f = W
    GOTO TWO; NO
    MOVLW VARLCD1 ; YES
    RETURN
    TWO
    MOVLW 0x02
    CPFSEQ DIVISION_MODULO ; SKIP if f = W
    GOTO THREE; NO
    MOVLW VARLCD2 ; YES
    RETURN
    THREE
    MOVLW 0x03
    CPFSEQ DIVISION_MODULO ; SKIP if f = W
    GOTO FOURTH; NO
    MOVLW VARLCD3 ; YES
    RETURN
    FOURTH
    MOVLW 0x04
    CPFSEQ DIVISION_MODULO ; SKIP if f = W
    GOTO FIVE; NO
    MOVLW VARLCD4 ; YES
    RETURN
    FIVE
    MOVLW 0x05
    CPFSEQ DIVISION_MODULO ; SKIP if f = W
    GOTO TWO; NO
    MOVLW VARLCD5 ; YES
    RETURN
    SIX
    MOVLW 0x06
    CPFSEQ DIVISION_MODULO ; SKIP if f = W
    GOTO SEVEN; NO
    MOVLW VARLCD6 ; YES
    RETURN
    SEVEN
    MOVLW 0x07
    CPFSEQ DIVISION_MODULO ; SKIP if f = W
    GOTO HEIGHT; NO
    MOVLW VARLCD7 ; YES
    RETURN
    HEIGHT
    MOVLW 0x08
    CPFSEQ DIVISION_MODULO ; SKIP if f = W
    GOTO NEINE; NO
    MOVLW VARLCD8 ; YES
    RETURN
    NEINE
    MOVLW 0x09
    CPFSEQ DIVISION_MODULO ; SKIP if f = W
    GOTO ENDING; NO
    MOVLW VARLCD9 ; YES
    ENDING
    RETURN
    
DISPLAY
    ; 987 % 10 = 7
    ; DISPLAY 7
    ; RA0
    
    MOVFF NUMBER_7_SEGMENTS, DIVISION_NUMERATOR
    MOVLW 0x0A ; 10
    MOVWF DIVISION_DENOMINATOR
    CALL DIVISION
    CALL DISPLAY_DECODER
    MOVWF PORTD
    BSF PORTA, RA0
    
    CALL DELAY
    
    ; Result = (987 - 7) / 10 = 98
    ; 98 % 10 = 8
    ; DISPLAY 8
    ; RA1
    
    
    BSF PORTA, RA1
    CALL DELAY
    
    BSF PORTA, RA2
    CALL DELAY
    
    BSF PORTA, RA3
    CALL DELAY
    
    ; Result = (98 - 8) % 10 = 9
    ; 9 % 10 = 9
    ; DISPLAY 9
    ; RA2
    
    ; Result = (9 - 9) / 10
    ; 0 % 10 = 0
    ; DISPLAY 0
    ; RA3
    
    ;MOVLW VARLCD1
    ;MOVWF PORTD
    ;BSF PORTA, RA3

    ;CALL DELAY
    
    RETURN
    
    
_DISPLAY
    MOVLW VARLCD3
    MOVWF PORTD
    BSF PORTA, RA2
    CALL DELAY
    RETURN

DIVISION
    CLRF DIVISION_RESULT ; Init Result
    MOVFF DIVISION_NUMERATOR, REGA ; REGA will remplace DIVISION_NUMERATOR because we need it after
    DIVISION_LOOP
    MOVF DIVISION_DENOMINATOR, WREG ; Loading DIVISION_DENOMINATOR inside the WREG
    SUBWF REGA, W ; f - w => WREG
    BTFSC STATUS, OV ; Negatif ? Skip if Clear
    GOTO MODULO ; Yes
    INCF DIVISION_RESULT, F ; No
    MOVWF REGA ; WREG => REGA for the new calculus in the next iteration
    GOTO DIVISION_LOOP
    MODULO
    MOVFF REGA, DIVISION_MODULO
    RETURN

_YES
    BSF PORTE, RE0
    GOTO MAIN
    
MAIN
    
    ;MOVLW 0x87
    ;MOVWF DIVISION_NUMERATOR
    ;MOVLW 0x0A
    ;MOVWF DIVISION_DENOMINATOR
    ;CALL DIVISION
    
    MOVLW 0x89 ;(140)
    MOVWF REGA
    MOVLW 0x0A
    BCF STATUS, N
    SUBWF REGA, W ; f - w => WREG / 0x64 - 0x0A = 0x90
    BTFSC STATUS, N ; Negatif ? Skip if Clear
    GOTO _YES ; Negatif
    BSF PORTE, RE1 ; Positif
    GOTO MAIN
    
    
   ;MOVLW 0x18
   ;MOVWF NUMBER_7_SEGMENTS
    
   ;CALL DISPLAY
    
    ;MOVLW 0x8A ; 1000
    ;MOVWF DIVISION_NUMERATOR
    ;MOVLW 0x0A ; 300
    ;MOVWF DIVISION_DENOMINATOR
    ;CALL DIVISION
    
    
    GOTO MAIN
    END

; 100 / 10 = 0x0A => STATUS = 0x10
; 110 / 10 = 0x0B => STATUS = 0x10
; 120 / 10 = 0x0C => STATUS = 0x10 
; 125 / 10 = 0x0C => STATUS = 0x10
; 127 / 10 = 0x0C => STATUS = 0x10
; 128 / 10 = 0x0C => STATUS = 0x10
; 129 / 10 = 0x0C => STATUS = 0x10
; 130 / 10 = 0x0D => STATUS = 0x10
; 131 / 10 = 0x0D => STATUS = 0x10
; 135 (0x87) / 10 = 0x0D => STATUS = 0x10
; 136 (0x88) OK
; 137 (0x89) OK   
; 138(0x8A) / 10 = 0x00 => STATUS = 0x13 (NOT OK)
; 139 / 10 = 0x00 => STATUS = 0x13 (NOT OK)
; 140 / 10 = 0x00 => STATUS = 0x13D (NOT OK)