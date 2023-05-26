; ISR_example.asm: a) Increments/decrements a BCD variable every half second using
; an ISR for timer 2; b) Generates a 2kHz square wave at pin P1.1 using
; an ISR for timer 0; and c) in the 'main' loop it displays the variable
; incremented/decremented using the ISR for timer 2 on the LCD.  Also resets it to 
; zero if the 'BOOT' pushbutton connected to P4.5 is pressed.
$NOLIST
$MODLP51RC2
$LIST

CLK           EQU 22118400 ; Microcontroller system crystal frequency in Hz
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (pk amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))

; buttons ordered from top to bottom (or right to left if facing LCD)
SET_ALARM     equ P0.0
SOUND_OUT     equ P1.1 ; this is across SET_ALARM (the side with 1 button)
a_INC_MIN     equ P0.4
a_INC_HOUR	  equ P0.6
INC_MIN 	  equ P4.5
INC_HOUR	  equ P2.4


; Reset vector
org 0x0000
    ljmp main

; External interrupt 0 vector (not used in this code)
org 0x0003
	reti

; Timer/Counter 0 overflow interrupt vector
org 0x000B
	ljmp Timer0_ISR

; External interrupt 1 vector (not used in this code)
org 0x0013
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 0x001B
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR

; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
dseg at 0x30 ; The DSEG statement selects an absolute segment within DATA space - 0x30 cannot be moved
Count1ms:     ds 2 ; Used to determine when half second has passed - Count1ms is an absolute data word variable (constant)
BCD_counter:  ds 1 ; The BCD counter incrememted in the ISR and displayed in the main loop - BCD_counter is an absolute data word variable (constant)
min_counter: ds 1 
hour_Counter: ds 1
a_min_counter: ds 1
a_hour_counter: ds 1

; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg
half_seconds_flag: dbit 1 ; this is really a seconds_flag now
M_Flag: dbit 1 
M_Char: dbit 1
a_M_flag: dbit 1
a_M_char: dbit 1
o_Flag:   dbit 1

cseg
; These 'equ' must match the hardware wiring
LCD_RS equ P3.2
;LCD_RW equ PX.X ; Not used in this code, connect the pin to GND
LCD_E  equ P3.3
LCD_D4 equ P3.4
LCD_D5 equ P3.5
LCD_D6 equ P3.6
LCD_D7 equ P3.7

$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$LIST

;                     1234567890123456    <- This helps determine the location of the counter
Initial_Message:  db 'Time XX:XX:XX #M', 0
Line_2_Message:   db 'Alm XX:XX #M o$$', 0
a_M_char_ON: 	  db              'ON ', 0
a_M_char_OFF:     db              'OFF', 0

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init:
	mov a, TMOD ; TMOD is an 8-bit register containing Timer 1 and Timer 0: GATE, C/T, M1, M0 (look at slides for more details)
	anl a, #0xf0 ; 11110000 Clear the bits for timer 0
	orl a, #0x01 ; 00000001 Configure timer 0 as 16-bit timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD) ; TH0 is the MSBs to the left half of TIMER0_RELOAD
	mov TL0, #low(TIMER0_RELOAD) ; TL0 is the LSBs to the right half of TIMER0_RELOAD
	; Set autoreload value
	mov RH0, #high(TIMER0_RELOAD)
	mov RL0, #low(TIMER0_RELOAD) 
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt - setb just turns something to 1
    setb TR0  ; Start timer 0
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P1.1 ;
;---------------------------------;
Timer0_ISR:
	;clr TF0  ; According to the data sheet this is done for us already. -- clr sets something to 0
	cpl SOUND_OUT ; Connect speaker to P1.1! - cpl logically complements the current value
	reti ; reti ends the ISR (pops the high and low order bytes of the PC)

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
;---------------------------------;
Timer2_Init:
	; initialize alarm flag and alarm AMPM
	mov hour_counter, #0x12
	mov a_M_Char, #'A'
	mov M_Char, #'A'
	mov M_Flag, #0x1
	mov T2CON, #0
	mov TH2, #high(TIMER2_RELOAD)
	mov TL2, #low(TIMER2_RELOAD)
	; Set the reload value
	mov RCAP2H, #high(TIMER2_RELOAD)
	mov RCAP2L, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a ; set register a to 0
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Enable the timer and interrupts
    setb ET2  ; Enable timer 2 interrupt
    setb TR2  ; Enable timer 2
	ret

;---------------------------------;
; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in ISR -- TF2 is the overflow flag
	cpl P1.0 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	 
	; Increment the 16-bit one mili second counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1
	
Inc_Done:
	; Check if half second has passed
	mov a, Count1ms+0
	cjne a, #low(1000), Timer2_ISR_done ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(1000), Timer2_ISR_done
	
	; 500 milliseconds have passed.  Set a flag so the main program knows
	setb half_seconds_flag ; Let the main program know half second had passed
	
	cpl TR0 ; Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
	clr a ; Reset to zero the milli-seconds counter, it is a 16-bit variable
	mov Count1ms+0, a
	mov Count1ms+1, a
	
	; Increment the BCD counter
	mov a, BCD_counter
	jnb INC_MIN, inc_min_2
	jnb INC_HOUR, inc_hour_2
	add a, #0x01
	
Timer2_ISR_da:
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov BCD_counter, a
	
Timer2_min_increment:	
	; increment min_Counter
	mov b, #90 ; ASK IN LAB
	div ab
	jnz inc_min_Counter
	sjmp Timer2_ISR_done
	
inc_min_Counter:
	mov BCD_Counter, #0x00
	
inc_min_2:
	mov a, min_Counter
	add a, #0x01
	da a
	mov min_Counter, a
	mov b, #90
	div ab
	jnz inc_hour_counter
	sjmp Timer2_ISR_done
	
inc_hour_counter:
	mov min_Counter, #0x00
	
inc_hour_2:
	mov a, hour_counter
	add a, #0x01
	da a
	mov hour_counter, a
	cjne a, #0x12, hour_12_check
	
SW_M_Flag:
	cpl M_Flag
	jb M_Flag, AM
	mov M_Char, #'P'
	sjmp hour_12_check
	
AM:	
	mov M_Char, #'A'
	
hour_12_check:
	mov a, hour_counter
	mov b, #19
	div ab
	jnz reset
	sjmp Timer2_ISR_done
	
reset:
	mov hour_counter, #0x01
	
Timer2_ISR_done:
	pop psw
	pop acc
	reti

;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
main:
	; Initialization
    mov SP, #0x7F
    lcall Timer0_Init
    lcall Timer2_Init
    ; In case you decide to use the pins of P0, configure the port in bidirectional mode:
    mov P0M0, #0
    mov P0M1, #0
    setb EA   ; Enable Global interrupts
    lcall LCD_4BIT
    ; For convenience a few handy macros are included in 'LCD_4bit.inc':
	Set_Cursor(1, 1)
    Send_Constant_String(#Initial_Message)
    Set_Cursor(2,1)
    Send_Constant_String(#Line_2_Message)
    
    setb half_seconds_flag
	mov BCD_counter, #0x00
	mov a_min_counter, #0x00
	mov a_hour_counter,  #0x12
    
set_alarm_initial:
	; display alarm as OFF initially
    Set_Cursor(2,14)
	Send_Constant_String(#a_M_char_OFF)
	;display alarm as AM initially 
    Set_Cursor(2,11)
	Display_char(#'A')
	; display alarm minutes as 0x00 initially
	Set_Cursor(2,8)
	Display_BCD(a_min_counter)
	; display alarm hour as 0x12 initially
	Set_Cursor(2,5)
	Display_BCD(a_hour_counter)
    
    
; strategy: once everything has been initialized: we're going to continue checking if the alarm should be triggered
	
one_800_267_2001_ALARM_FORCE:	

loop_b:
    clr half_seconds_flag ; We clear this flag in the main loop, but it is set in the ISR for timer 2
    
    ; display timer char, sec, min, hour 
    ;char
	Set_Cursor(1, 15)
    Display_char(M_Char)
    ; sec
    Set_Cursor(1, 12)
	Display_BCD(BCD_counter)
	; min
	Set_Cursor(1, 9)
	Display_BCD(min_counter)
	; hour
    Set_Cursor(1, 6)
	Display_BCD(hour_Counter)

    
    ; display alarm char, min, hour 
    ; char
	Set_Cursor(2,11)
	Display_char(a_M_Char)
	; min
	Set_Cursor(2,8)
	Display_BCD(a_min_counter)
	; hour
	Set_Cursor(2,5)
	Display_BCD(a_hour_counter)
	

; check if alarm minute and alarm hour buttons are being pressed
a_min_check:	
	jb a_INC_MIN, a_hour_check
	Wait_Milli_Seconds(#50)
	jb a_INC_MIN, a_hour_check
	jnb a_INC_MIN, $
	sjmp a_inc_min_2
	
a_hour_check:
	jb a_INC_HOUR, a_set
	Wait_Milli_Seconds(#50)
	jb a_INC_HOUR, a_set
	jnb a_INC_HOUR, $
	sjmp a_inc_hour_2

a_inc_min_2:
	mov a, a_min_counter
	add a, #0x01
	da a
	mov a_min_counter, a
	mov b, #90
	div ab
	jnz a_min_reset
	sjmp a_hour_check
	
	
	

a_min_reset:
	mov a_min_counter, #0x00
	
a_inc_hour_2:
	mov a, a_hour_counter
	add a, #0x01
	da a
	mov a_hour_counter, a
	mov b, #18
	cjne a, #0x12, a_hour_12_check
	
a_SW_M_Flag:
	cpl a_M_Flag
	; distinguish between AM and PM
	jb a_M_Flag, a_AM
	; if AM, then display PM
	mov a_M_Char, #'P'
	sjmp a_hour_12_check
	
a_AM:
	mov a_M_Char, #'A'
	
a_hour_12_check:
	mov a, a_hour_counter
	mov b, #19
	div ab
	jnz a_reset
	sjmp a_min_check
	
a_reset:
	mov a_hour_counter, #0x01	
	
a_set:
	jb SET_ALARM, sound_controller_sequence
	Wait_Milli_Seconds(#50)
	jb SET_ALARM, sound_controller_sequence
	jnb SET_ALARM, $
	cpl O_Flag
	jb O_Flag, SET_ALARM_DISPLAY_ON
	sjmp SET_ALARM_DISPLAY_OFF
	
SET_ALARM_DISPLAY_OFF:
	Set_Cursor(2,14)
	Send_Constant_String(#a_M_char_OFF)
    sjmp sound_controller_sequence    

SET_ALARM_DISPLAY_ON:
	Set_Cursor(2,14)
	Send_Constant_String(#a_M_char_ON)
	sjmp  sound_controller_sequence
    
sound_controller_sequence:

compare_sound_minutes:
	mov a, min_counter
	mov b, a_min_counter
	cjne a, b, sound_off

compare_sound_hours:
	mov a, hour_Counter
	mov b,  a_hour_counter
	cjne a, b, sound_off
	
compare_OM_flags:
	jnb O_Flag, sound_off
	
; now to figure out if AM and PM match and if alarm sound is going to go off:

compare_sound_on_PM:
	jnb M_Flag, sound_A_on

go_sound_on_AM:
	sjmp sound_P_on
	
sound_P_on:
	jnb a_M_Flag, sound_off
	
sound_on:
	setb ET0
	ljmp loop_b
	
sound_A_on:
	jb a_M_Flag, sound_off
	sjmp sound_on
	
sound_off:
	clr ET0	
	ljmp one_800_267_2001_ALARM_FORCE

END


