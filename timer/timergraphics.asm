;;; Produce nice random graphics using the timer.

;;; Timer is CIA 1, timer B
	timer = $DC06
	timerc = $DC0F
;;; See https://www.c64-wiki.de/wiki/CIA
	CLRSCRCOL = $82
	TIMESTART = $5555

	;; Zeropage (16bit); timer value
	ZPTIMEVAL = $15

	* = $0801-2

	.word $0801
	.word eobas
	.word 38911
	.byte $9e		; SYS
	.asc "2063"		; This is a prime number.
eobas:	.byte 0,0,0

	nop
	nop
	
main:	sei			; Disable interrupts.
	lda #8			; Color 8 is orange into border and background.
	sta $d020
	sta $d021
	lda #$3b		; Hires
	sta $d011
	lda $d018		; @$2000
	ora #8
	sta $d018
	jsr clrscr		; This clears the textmode screen and sets the hires colours.
	jsr inittimer		; Initialise the timer.
loop:	lda timer		; Get current value of timer (low byte).
p1:	sta $2000		; Write it into the graphics area.
	lda timer+1		; Get high byte of timer.
p2:	sta $2001		; And again write it into the graphics area.
	lda #2			; Now add two to both pointers p1 and p2.
	clc
	adc p1+1		; Here pointer p1.
	sta p1+1
	bcc noov1
	inc p1+2		; We have a carry, ergo the high byte must be incremented.
noov1:	lda #2
	clc
	adc p2+1		; Here pointer p2.
	sta p2+1
	bcc noov2
	inc p2+2
noov2:	lda p1+2
	cmp #$40		; Have we reached the end of the graphics area?
	bne skp1		; No, so skip.
	lda #$20
	sta p1+2		; Write $20 to both high bytes of the pointers.
	sta p2+2
skp1:
	jsr checkjoy
	jmp loop		; This is an endless loop, sorry.

;;; Clear the text screen.
clrscr:	.(
	lda #CLRSCRCOL
	ldx #0
l1	sta $0400,x
	sta $0500,x
	sta $0600,x
	sta $0700,x
	dex
	bne l1
	rts
	.)

;;; Set timer to $ffff to have the maximum runtime possible.
inittimer:
	lda #<TIMESTART
	sta timer		; Set timer.
	sta ZPTIMEVAL		; Set our own counter value.
	lda #>TIMESTART
	sta timer+1
	sta ZPTIMEVAL+1
	lda #1			; Start the timer, it will repeat.
	sta timerc
	rts

checkjoy:	.(
	; lda $d012		; Get rasterposition.
	; cmp #$91		; A random value.
	; bne out
	; inc $d020
        lda #%00000001		; Joystick UP.
        bit $dc00		; CIA 1, Joystick in Port 2.
        bne no1			; If joystick pushed up, then pin set to ground.
        inc ZPTIMEVAL		; Increment the current timer value variable.
	bne no1
	inc ZPTIMEVAL+1		; And the high byte on overflow, too.
no1:	lda #%00000010		; Joystick DOWN.
        bit $dc00
        bne no2
	ldx ZPTIMEVAL
	dex
	stx ZPTIMEVAL
	cpx #$ff
	bne no2
	dec ZPTIMEVAL+1
no2:	lda ZPTIMEVAL
	sta timer		; Set timer.
	lda ZPTIMEVAL+1
	sta timer+1		; Set timer.
	; dec $d020
out:	rts
	.)
