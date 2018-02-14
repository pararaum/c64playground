	;; Start two bytes earlier so that we can add the load address...
	* = $c000 - 2
	.byte $00, $c0

	jmp install
	.db	0, 0

install	sei
	lda $0314		;save old pointer
	sta irqptr
	lda $0315
	sta irqptr+1
	lda #<irqr
	sta $0314
	lda #>irqr
	sta $0315
	cli
	rts
	brk
	brk
	
irqptr:	.word $ea31
irqr:	nop
	dec $d020
	ldy #39
cploop	lda $0100,y
	sta $0400,y
	dey
	bpl cploop
	tsx
	txa
	jsr a2hex
	stx $0428
	sty $0429
	inc $d020
	jmp (irqptr)

;;; Convert accumulator to hex petscii in x/y
a2hex:	pha
	and #$0f
	cmp #10
	bcc *+4
	sbc #48 + 10 - 1
	clc
	adc #48
	tay
	pla
	lsr
	lsr
	lsr
	lsr
	cmp #10
	bcc *+4
	sbc #48 + 10 - 1
	clc
	adc #48
	tax
	rts
