	.include "t7d/stackmacros.i"
	.include "t7d/memoryfunctions.i"

	.importzp	ptr1

clear_memory:
	pha			; LO of size
	lda	#0
	tay
	cpx	#0
	beq	pages_finished
l1:	sta	(ptr1),y
	iny
	bne	l1
	;; Y = 0
	inc	ptr1+1		; Increment hi of dest
	dex
	bne	l1
pages_finished:
	pla			; LO of size
	tax			; Put number of bytes into X.
	beq	out		; No LO, quit.
	;; Y is still zero!
	tya			; Clear A again.
l2:	sta	(ptr1),y	; Clear memory.
	iny			; Increment index to next byte.
	dex			; Decrement byte counter.
	bne	l2		; Finished?
out:	rts
