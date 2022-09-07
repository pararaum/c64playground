	.include	"t7d/memoryfunctions.i"
	.importzp	ptr1,ptr2

	.export	copy_song

	.segment	"ONCE"
;;; Copy the song to the address it belongs to.
;;; Input: ptr1=Pointer to song (load address), A/X=size
;;; Output: -
;;; Modifies: A,X,Y,ptr1,ptr2
copy_song:
	pha			; size LO
	txa			; size HI
	pha
	ldy	#0
	lda	(ptr1),y	; destination LO
	sta	ptr2
	iny
	lda	(ptr1),y	; destination HI
	sta	ptr2+1
	lda	ptr1		; Skip the load address.
	clc
	adc	#2
	sta	ptr1
	lda	ptr1+1
	adc	#0
	sta	ptr1+1
	pla			; size HI
	tax
	pla			; size LO
	jmp	memcpy_up
