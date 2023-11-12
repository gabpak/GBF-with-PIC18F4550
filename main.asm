#include <p18F4550.inc>

CONFIG WDT=OFF ; disable watchdog timer
CONFIG MCLRE = ON ; MCLEAR Pin on
CONFIG CPUDIV =  OSC1_PLL2;
CONFIG PLLDIV = 1;
CONFIG DEBUG = OFF ; Disable Debug Mode
CONFIG FOSC = HS ; oscillator mode
CONFIG PBADEN = OFF
    
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
VARLCDE equ b'1000000' ; Error
	
; MEMORY EMPLACEMENT
REGA		equ 0x01
REGB		equ 0x02
REGC		equ 0x03
FLAG_16BIT__N	equ 0x04
	
COUNT	equ 0xFF
		
DIVISION_NUMERATOR	equ 0x0A ; The numerator of the division
DIVISION_DENOMINATOR	equ 0x0B ; The denominator of the division
DIVISION_RESULT		equ 0x0C ; The result of the division
DIVISION_MODULO		equ 0x0D ; The rest of the euclidian division
	
NUMBER_7_SEGMENTS	equ 0x10 ; The number to display on the 7-segments

; 16 bits memory
OctetA_H    equ 0x20 ; A: 00000000 xxxxxxxx
OctetA_L    equ 0x21 ; A: xxxxxxxx 00000000
    
OctetB_H    equ 0x22 ; B: 00000000 xxxxxxxx
OctetB_L    equ 0x23 ; B: xxxxxxxx 00000000 
    
OctetC_H    equ 0x24 ; C: 00000000 xxxxxxxx
OctetC_L    equ 0x25 ; C: xxxxxxxx 00000000
    
OctetD_H    equ 0x26 ; D: 00000000 xxxxxxxx
OctetD_L    equ 0x27 ; D: xxxxxxxx 00000000
    
OctetE_H    equ 0x28 ; D: 00000000 xxxxxxxx
OctetE_L    equ 0x29 ; D: xxxxxxxx 00000000
    
OctetF_H    equ 0x2A ; D: 00000000 xxxxxxxx
OctetF_L    equ 0x2B ; D: xxxxxxxx 00000000
    
OctetG_H    equ 0x2C ; D: 00000000 xxxxxxxx
OctetG_L    equ 0x2D ; D: xxxxxxxx 00000000


; Program START
    
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
    CLRF FLAG_16BIT__N
    
    ; 8 Bit division
    CLRF DIVISION_NUMERATOR
    CLRF DIVISION_DENOMINATOR
    CLRF DIVISION_RESULT
    CLRF DIVISION_MODULO
    
    ; 16 Bit register
    CLRF OctetA_L
    CLRF OctetA_H
    CLRF OctetB_L
    CLRF OctetB_H
    CLRF OctetC_L
    CLRF OctetC_H
    CLRF OctetD_L
    CLRF OctetD_H
    CLRF OctetE_L
    CLRF OctetE_H
    CLRF OctetF_L
    CLRF OctetF_H
    CLRF OctetG_L
    CLRF OctetG_H
    
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
    BTFSC PIR1, TMR2IF
    GOTO TMR2_IF
    RETFIE ; No
 
AD_CONVERSION
    BCF PIR1, ADIF ; Init back ADIF to 0x00
    MOVFF ADRESH, PORTB ; Start the conversion here ..
    MOVF PORTB, WREG
    ;MOVWF DIVISION_NUMERATOR
    ;MOVLW 0x02
    ;MOVWF DIVISION_DENOMINATOR
    ;CALL DIVISION
    ;MOVF DIVISION_RESULT, WREG
    MOVWF NUMBER_7_SEGMENTS
    RETFIE
    
TMR2_IF ; Flag TMR2IF Raised
    BCF PIR1, TMR2IF
    RETFIE

DELAY    ; Delay function that use the global variable COUNT
    DECFSZ COUNT
    GOTO DELAY
    CALL RESET_COUNT_DELAY_1
    RETURN
RESET_COUNT_DELAY_1
    MOVLW COUNT_TIMER
    MOVWF COUNT
    CALL DELAY_2
    RETURN
DELAY_2
    DECFSZ COUNT
    GOTO DELAY_2
    CALL RESET_COUNT_DELAY_2
    RETURN
RESET_COUNT_DELAY_2
    MOVLW COUNT_TIMER
    MOVWF COUNT
    RETURN
    
DIVISION ; 8bit division (137 is the limit here)
    ; NUMERATEUR
    CLRF DIVISION_RESULT ; Init Result
    MOVFF DIVISION_NUMERATOR, REGA ; REGA will remplace DIVISION_NUMERATOR because we need it after
    DIVISION_LOOP
    MOVF DIVISION_DENOMINATOR, WREG ; Loading DIVISION_DENOMINATOR inside the WREG
    SUBWF REGA, W ; f - w => WREG
    BTFSC STATUS, N ; Negatif ? Skip if Clear
    GOTO MODULO ; Yes
    INCF DIVISION_RESULT, F ; No
    MOVWF REGA ; WREG => REGA for the new calculus in the next iteration
    GOTO DIVISION_LOOP
    MODULO
    MOVFF REGA, DIVISION_MODULO
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
    GOTO SIX; NO
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
    MOVLW VARLCDE
    RETURN
    
DISPLAY
    MOVFF NUMBER_7_SEGMENTS, REGC
    MOVFF NUMBER_7_SEGMENTS, DIVISION_NUMERATOR
    MOVLW 0x0A
    MOVWF DIVISION_DENOMINATOR
    CALL DIVISION
    CALL DISPLAY_DECODER
    MOVWF PORTD
    BSF PORTA, RA0
    
    CALL DELAY
    
    MOVFF DIVISION_MODULO, WREG
    SUBWF REGC, W
    MOVWF DIVISION_NUMERATOR
    MOVLW 0x0A
    MOVWF DIVISION_DENOMINATOR
    CALL DIVISION
    ; Je récup le resultat de la division
    ; je calcul le modulo en refaisant la division avec le nouveau résultat
    MOVFF DIVISION_RESULT, DIVISION_NUMERATOR
    MOVLW 0x0A
    MOVWF DIVISION_DENOMINATOR
    CALL DIVISION
    CALL DISPLAY_DECODER
    MOVWF PORTD
    BSF PORTA, RA1
    
    CALL DELAY
    
    MOVFF DIVISION_MODULO, WREG
    SUBWF DIVISION_NUMERATOR, W
    MOVWF DIVISION_NUMERATOR
    MOVLW 0x0A
    MOVWF DIVISION_DENOMINATOR
    ; Je récup le resultat de la division
    ; je calcul le modulo en refaisant la division avec le nouveau résultat
    MOVFF DIVISION_RESULT, DIVISION_NUMERATOR
    MOVLW 0x0A
    MOVWF DIVISION_DENOMINATOR
    CALL DIVISION
    CALL DISPLAY_DECODER
    MOVWF PORTD   ; Affiche 2
    BSF PORTA, RA2
    
    CALL DELAY
    
    MOVFF DIVISION_MODULO, WREG
    SUBWF DIVISION_NUMERATOR, W
    MOVWF DIVISION_NUMERATOR
    MOVLW 0x0A
    MOVWF DIVISION_DENOMINATOR
    ; Je récup le resultat de la division
    ; je calcul le modulo en refaisant la division avec le nouveau résultat
    MOVFF DIVISION_RESULT, DIVISION_NUMERATOR
    MOVLW 0x0A
    MOVWF DIVISION_DENOMINATOR
    CALL DIVISION
    CALL DISPLAY_DECODER
    MOVWF PORTD
    BSF PORTA, RA3  
    CALL DELAY
    RETURN

COMPARE_16BITS_C_AND_B ; 16 bits comparator C - B and raise flag
    MOVF OctetB_H, WREG
    SUBWF OctetC_H, WREG ; C - B => WREG
    BTFSS STATUS, Z ; Si Z = 1 alors SKIP
    GOTO RESULT; Z = 0
    MOVF OctetB_L, WREG; Z = 1
    SUBWF OctetC_L, WREG ; F - W / 
    RESULT
        ; if A=B then now Z=1.
        ; if A<B then now C=0.
        ; if A<=B then now C=1.
    RETURN
    
SUBTRACT_16BITS
    MOVF OctetB_L, WREG
    SUBWF OctetA_L , WREG
    MOVWF OctetC_L

    MOVF OctetB_H, WREG
    SUBWFB OctetA_H, WREG
    MOVWF OctetC_H
    
    RETURN
    
 ; Substract C - B => C
SUBTRACT_C_MINUS_B_TO_C_16BITS
    MOVF OctetB_H, WREG
    SUBWF OctetC_H, WREG ; C - B => REG
    MOVWF OctetC_H ; W => C

    MOVF OctetB_L, WREG
    SUBWF OctetC_L , WREG
    MOVWF OctetC_L

    RETURN
       
; Increment 16 bits value in OctetA
INCREMENT_A_16BITS
    INCF OctetA_L, F ; Increment OctetA_L
    BTFSS STATUS, Z ; If Z = 1
    GOTO INCREMENT_A_DONE ; Z = 1
    INCF OctetA_H, F ; Z = 0
    INCREMENT_A_DONE
    RETURN
    
INCREMENT_B_16BITS
    INCF OctetB_L, F ; Increment OctetA_L
    BTFSS STATUS, Z ; If Z = 1
    GOTO INCREMENT_B_DONE ; Z = 1
    INCF OctetB_H, F ; Z = 0
    INCREMENT_B_DONE
    RETURN
    
INCREMENT_C_16BITS
    INCF OctetC_L, F ; Increment OctetA_L
    BTFSS STATUS, Z ; If Z = 1
    GOTO INCREMENT_C_DONE ; Z = 1
    INCF OctetC_H, F ; Z = 0
    INCREMENT_C_DONE
    RETURN
    
INCREMENT_D_16BITS
    INCF OctetD_L, F ; Increment OctetA_L
    BTFSS STATUS, Z ; If Z = 1
    GOTO INCREMENT_D_DONE ; Z = 1
    INCF OctetD_H, F ; Z = 0
    INCREMENT_D_DONE
    RETURN
    
    ; DECREMENT PART
    
; Decrement 16 bits value in OctetA
DECREMENT_A_16BITS
    DECF OctetA_L, F ; Decrement OctetA_L
    BTFSS STATUS, N ; If N = 1 Then SKIP
    GOTO DECREMENT_A_DONE ; N = 0
    DECF OctetA_H, F ; N = 1
    DECREMENT_A_DONE
    RETURN
    
DECREMENT_B_16BITS
    DECF OctetB_L, F ; Decrement OctetA_L
    BTFSS STATUS, N ; If N = 1 Then SKIP
    GOTO DECREMENT_B_DONE ; N = 0
    DECF OctetB_H, F ; N = 1
    DECREMENT_B_DONE
    RETURN

DECREMENT_C_16BITS
    DECF OctetC_L, F
    BTFSS STATUS, N ; If N = 1 Then SKIP
    GOTO DECREMENT_C_DONE ; N = 0
    DECF OctetC_H, F ; N = 1
    DECREMENT_C_DONE
    RETURN
    
; Decrement 16 bits value in OctetA
DECREMENT_D_16BITS
    DECF OctetD_L, F ; Decrement OctetA_L
    BTFSS STATUS, N ; If N = 1 Then SKIP
    GOTO DECREMENT_D_DONE ; N = 0
    DECF OctetD_H, F ; N = 1
    DECREMENT_D_DONE
    RETURN

    
DIVIDE_LOOP_16BITS ; OctetD is the div result / OctectE is the modulo
    CLRF OctetC_L
    CLRF OctetC_H
    CLRF OctetD_L
    CLRF OctetD_H
    CLRF OctetE_L
    CLRF OctetE_H
    CLRF OctetF_L
    CLRF OctetF_H
    CLRF OctetG_L
    CLRF OctetG_H
    
    ; A = Numerator
    ; B = Denominator
    ; C = Reslult of the substraction
    ; D = Result of the division
    ; E = Modulo
    ; F = Register
    ; G = Register
    
    ; Ne doivent pas changer
    ; Numerator OctetA_H / OctetA_L
    ; Denominator OctetB_H / OctetB_L
    
    MOVFF OctetA_H, OctetC_H
    MOVFF OctetA_L, OctetC_L
    
    INTERN_DIVIDE_LOOP_16BITS
    CALL SUBTRACT_C_MINUS_B_TO_C_16BITS ; C - B => C
    CALL COMPARE_16BITS_C_AND_B ; Put the flag
    BTFSC STATUS, N ; If negatif
    GOTO FINISH_LOOP; Yes
    CALL INCREMENT_D_16BITS ; No
    GOTO INTERN_DIVIDE_LOOP_16BITS
    MOVFF OctetC_H, OctetE_H ; Modulo
    MOVFF OctetC_L, OctetE_L ; Modulo
    FINISH_LOOP
    RETURN


MAIN
    MOVLW 0x00
    MOVWF OctetA_H
    MOVLW 0xBB
    MOVWF OctetA_L
    ; 45
    
    MOVLW 0x00
    MOVWF OctetB_H
    MOVLW 0x14
    MOVWF OctetB_L
    ; 10

    CALL DIVIDE_LOOP_16BITS
    
    ;BSF ADCON0, GO/DONE
    ;CALL DISPLAY

    GOTO MAIN
    END
    
