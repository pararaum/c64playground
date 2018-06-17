	* = $c000-2

	.word $c000

	bitpattern = $02
	
	lda #3
	sta $fb
	lda #$AA
	sta bitpattern
	ldy #63
l1	tya
	jsr modulo
	cmp #2
	bne l2
	lda bitpattern
	eor #$ff
	sta bitpattern
l2	lda bitpattern
	sta $340,y		;write pattern to #13 sprite block
	dey
	bpl l1
	lda #13			;select #13 sprite block for sprite 0
	sta $7f8
	lda #100
	sta $d000
	sta $d001
	lda #$ff		;switch all sprites on
	sta $d015
l4	bmi l4
	rts
	
modulo	SEC			;http://rosettacode.org/wiki/Arithmetic/Integer#6502_Assembly
l3	SBC $fb
	BCS l3
	ADC $fb
	RTS
