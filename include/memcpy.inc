	.text
	;; Copy from ($fc) to ($fe), number of pages in A
MEMCPY:	.(
	pha
	txa
	pha
	tya
	pha
	tsx
	lda $0103,x		;Get old accumulator value from stack
	tax			;X contains number of pages
	ldy #$00
cpy1:	lda ($fc),y
	sta ($fe),y
	iny
	bne cpy1
	inc $fd
	inc $ff
	;; 	stx $d020
	dex
	bne cpy1
	pla
	tay
	pla
	tax
	pla
	rts
	.)
