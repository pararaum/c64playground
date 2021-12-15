
	.include	"t7d/stackmacros.i"
	.import		pushax, _write
	.importzp	ptr1
	
	.export _main

	.data
text:	.byte	"Mismatch!",10,0
text_end:

	.bss
target:	.res	32

	.code
	
_main:
	lda	#<text
	sta	ptr1
	lda	#>text
	sta	ptr1+1
	PushMemoryToStack	ptr1, text_end-text
	lda	#<target
	sta	ptr1
	lda	#>target
	sta	ptr1+1
	PullMemoryFromStack 	ptr1, text_end-text
	ldx	#text_end-text
l1:	lda	text,x
	cmp	target,x
	bne	error
	dex
	bpl	l1
	lda	#0
	rts
error:
	lda	#1
	ldx	#0
	jsr	pushax
	lda	#<text
	ldx	#>text
	jsr	pushax
	lda	#text_end-text
	ldx	#0
	jsr	_write
	lda	#$FF
	rts
