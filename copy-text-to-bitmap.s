	.include	"t7d/memoryconfig.i"
	.importzp	ptr1,ptr2,ptr3
	.importzp	tmp1,tmp2

	TEXTSCREEN=$400
	BITMAP=$2000
	CHARROM=$d000

	txtsrc=ptr1
	chrsrc=ptr2
	bmpdst=ptr3

	.segment	"INIT"
	.segment	"ONCE"
	
	.code
_main:	nop
	rts

	.proc	copy_char
	ldy	#0
	sty	chrsrc+1	; HI
	lda	(txtsrc),y
	.REPEAT	3
	asl
	rol	chrsrc+1
	.ENDREPEAT
	sta	chrsrc		; ptr2=8*character
	lda	chrsrc+1
	clc
	adc	#>CHARROM
	sta	chrsrc+1
	ldy	#8
l1:	lda	(chrsrc),y
	sta	(bmpdst),y
	dey
	bpl	l1
	rts
	.endproc
	
	.segment	"STARTUP"
	lda	#<TEXTSCREEN
	sta	txtsrc
	lda	#>TEXTSCREEN
	sta	txtsrc+1
	lda	#<BITMAP
	sta	bmpdst
	lda	#>BITMAP
	sta	bmpdst+1
	sei
	memoryconfig_charrom
convloop:
	jsr	copy_char
	;; txtsrc+=1
	inc	txtsrc		; LO of txtsrc
	bne	nocarry2
	inc	txtsrc+1	; HI of txtsrc
nocarry2:
	;; bmpdst+=8
	lda	bmpdst
	clc
	adc	#8
	sta	bmpdst
	bcc	nocarry
	inc	bmpdst+1
nocarry:
	cmp	#<(BITMAP+8000)	; A has still LO in it.
	bne	convloop
	lda	bmpdst+1
	cmp	#>(BITMAP+8000)
	bne	convloop
	memoryconfig_basic
	cli
	rts
