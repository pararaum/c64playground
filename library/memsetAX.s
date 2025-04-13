	.include	"zeropage.inc"

	.export	memsetAX

	.bss
arraysize:	.res	2

	.code
memsetAX:
	;; Set the destination pointer.
	sta	ptr1
	stx	ptr1+1
	pla
	sta	retaddrLO
	pla
	sta	retaddrHI

	pla
	sta	arraysize+1
	pla
	sta	arraysize

	;; Full pages first.
	tya			; Y had the fill value
	ldx	arraysize+1	; HI of array size
	ldy	#0
fullpage:
	sta	(ptr1),y
	dey
	bne	fullpage
	inc	ptr1+1
	dex
	bne	fullpage

	ldx	arraysize	; LO of array size
	beq	exit		; Done?
	ldy	#0
partialpage:
	sta	(ptr1),y
	iny
	dex
	bne	partialpage

exit:
	lda	#0
	retaddrHI=*-1
	pha
	lda	#0
	retaddrLO=*-1
	pha
	rts
