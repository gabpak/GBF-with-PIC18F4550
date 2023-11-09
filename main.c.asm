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
	
ADRESSH_TO_DECREMENT	equ 0x20 ; The variable to decrement for the rectangular signal
RECTANGLE_HIGH		equ 0x21	
		
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

    CLRF TRISD ; Utilisé pour l'affichage du 7 segment
    CLRF PORTD  
    
    CLRF TRISE ; Utilisé pour le signal rectangle
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
    
    CLRF ADRESSH_TO_DECREMENT
    CLRF RECTANGLE_HIGH
    
    MOVLW COUNT_TIMER
    MOVWF COUNT
  
    GOTO INIT_PWM

    ; ______________________________________________________________________
    
    
IRQ_HANDLE
    BTFSC PIR1, ADIF ; ADIF interrupt flag for A/D Converter
    GOTO AD_CONVERSION ; Yes
    BTFSC PIR1, TMR2IF
    GOTO TMR2_IF
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
    MOVLW 0x00
    CPFSEQ DIVISION_DENOMINATOR ; Skip if F = W
    GOTO CHECK_NUM_DEN ; F != W
    MOVLW 0x00 ; To modify as an error
    MOVWF DIVISION_RESULT ; F = W
    RETURN
    
    ; Check here the Num > Den
    ; CPFSGT Compare F with WREG / Skip if F > W
    CHECK_NUM_DEN
    MOVFF DIVISION_DENOMINATOR, WREG ; DENOMINATOR => W
    CPFSGT REGA ; REGA > WREG ? / NUMERATOR > DENOMINATOR ?
    GOTO FINISH ; No
    GOTO DIVISION_LOOP ; Yes
    FINISH
    MOVLW 0x00; No - The NUMERATOR < DENOMINATOR
    MOVWF DIVISION_RESULT
    RETURN
    
    DIVISION_LOOP
    MOVF DIVISION_DENOMINATOR, WREG ; Loading DIVISION_DENOMINATOR inside the WREG
    SUBWF REGA, W ; f - w => WREG / WREG - REGA => WREG
    BTFSC STATUS, N ; Negatif ? Skip if Clear // PROBLEM HERE 
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
    ; Now we make the difference between REGB & NUMERATOR
    MOVFF REGB, WREG ; We place REGB Accu inside WREG => W
    MOVFF DIVISION_NUMERATOR, REGA ; DIVISION_NUMERATOR => REGA
    SUBWF REGA ; REGA - W = DIVISION_NUMERATOR - REGB => REGA
    MOVFF REGA, DIVISION_MODULO
    RETURN
    
RECTANGULAR_SIGNAL
    ; On recupère le signal de ADRESH => WREG
    ; BEGIN
    ; On DECF la valeur de WREG
    ; Si 0 => Changement d'état du signal
    ; GOTO BEGIN
    ; Changer l'état du signal
    ; Si 1 = 0
    ; Si 0 = 1
    ; RETURN
    MOVFF ADRESH, ADRESSH_TO_DECREMENT
    START_DECR
    DECF ADRESSH_TO_DECREMENT
    MOVLW 0x00 ; We move 0x00 to WREG
    CPFSEQ ADRESSH_TO_DECREMENT ; Skip if W = F
    GOTO START_DECR; F != W
    MOVLW 0x00 ; F = W
    CPFSEQ RECTANGLE_HIGH
    GOTO PUT_LOW; On met à l'état bas ici
    GOTO TERMINATE
    PUT_LOW
    BCF PORTE, RE0
    MOVLW 0x00; On met à l'état haut ici
    MOVWF RECTANGLE_HIGH
    CALL DELAY
    RETURN
    TERMINATE
    BSF PORTE, RE0
    MOVLW 0x01; On met à l'état haut ici
    MOVWF RECTANGLE_HIGH
    CALL DELAY
    RETURN

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
    MOVWF T2CON ; Prescale à 16
    MOVLW b'00101100'
    MOVWF CCP1CON ; PWM mode and PWM Duty cycle 2 LSB at 10
    
    BSF T2CON, TMR2ON
    
    GOTO MAIN

    
MAIN
    ;BSF ADCON0, GO/DONE
    ;CALL RECTANGULAR_SIGNAL
    
_   GOTO MAIN
    END