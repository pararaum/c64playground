;;; Setup functions for screen, etc.
	.include	"vicmacros.i"
	.include	"libt7d.i"
	.import	PLAYERBACKGROUND
	.import	PLAYERFOREGROUND
	.import	PLAYERBORDER

	.export	setup

	.segment	"ONCE"
setup:
	lda	$2a6		; NTSC/PAL? PAL=1
	lsr			; Put LSB into Carry.
	ror			; Move into MSB
	ora	$dd0e		; CIA2 Timer A control register (Bit#7 TOD speed).
	sta	$dd0e		; Bit#7=1 if 50 Hz.
	lda	PLAYERBACKGROUND ; Set background colour.
	sta	$d020
	lda	PLAYERBORDER	; Set border colour.
	sta	$d021
	lda	PLAYERFOREGROUND	; Set foreground colour for whole screen.
	jsr	_fill_colour_ram
	;; Font is at $800.
	SetChargenAddress	$800
	lda	#0		; Clear CIA1 clock.
	sta	$dd0a
	sta	$dd09
	sta	$dd08
	rts
