	.export vic_init
	.import main
	.import charsetdata
	.import charsetaddr

CIAA = $dc00
Control_Timer_A = 14

vic_init:
	jsr	init_cia_tod
	lda	#7		; Yellow
	sta	$d020
	sta	$d021
	lda	#$9c		; Purple [http://sta.c64.org/cbm64pet.html]
	jsr	$ffd2
	lda	#$93		; Clear
	jsr	$ffd2
	;; Screen at $400, charset at $800
	lda	#(($0400/$0400)<<4)|<(charsetaddr/$0400)
	sta	$d018
	rts

init_cia_tod:
	lda	CIAA+Control_Timer_A
	ora	#$80		; 50Hz
	sta	CIAA+Control_Timer_A
	lda	#0
	ldy	#4-1
@l1:	sta	CIAA+8,y	; TOD
	dey
	bpl	@l1		; Write 1/10th of a second last to start clock.
	rts
