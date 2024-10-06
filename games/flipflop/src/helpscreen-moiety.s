	.include	"LAMAlib-macros16.inc"
	.include	"t7d/kernal.i"
	.include	"t7d/basic.i"
	.include	"t7d/pseudo/loadstorereg.i"

	.importzp	ptr1
	.import	_get_framecounter

	.export	_helpscreen

	.bss
endframe:	.res	2	; Ending frame, when this function is going to stop.

	.rodata
helptext:
	.incbin	"assets/helpscreen.seq"
	.byte	0
presstext:
	.byte	13,"press return for next page!",0

	.code
_helpscreen:
	ldax	#helptext
	stax	ptr1
fullloop:
	ldy	#0
loop:	lda	(ptr1),y
	beq	out
	inc16	ptr1
	jsr	CHROUT
	lda	$d6		; Get cursor row.
	cmp	#23
	bcc	loop
	Load16	A, Y, #presstext
	jsr	STROUT
	jsr	_get_framecounter
	addax	#601
	stax	endframe
inloop:
	jsr	_get_framecounter
	cmpax	endframe
	bcs	out
	jsr	GETIN		; Get key
	cmp	#$85
	beq	outFKey
	cmp	#$86
	beq	outFKey
	cmp	#$87
	beq	outFKey
	cmp	#$88
	beq	outFKey
	cmp	#13
	bne	inloop
	lda	#$93
	jsr	CHROUT
	jmp	fullloop
out:	ldax	#$FFFF
	rts
outFKey:			; Function key pressed this we will return.
	ldx	#0
	rts
