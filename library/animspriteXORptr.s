;;; Routine to perform a simple two frame animation.
	.include	"LAMAlib-macros16.inc"

	.importzp	ptr1, ptr2
	.macpack generic

	.export	animspriteXORptr

	.bss
spridx:	.res	1		; Current sprite index.
	
	.code
animspriteXORptr:
	stax	ptr2		; Store a copy of the pointer to the work area in ptr2.
	lda	#8-1		; 8 sprites to handle.
	sta	spridx
l1:	lda	spridx
	add	#8		; Index into Default values.
	tay
	lda	(ptr2),y
	beq	noanim		; Zero means, no animation at all.
	ldy	spridx		; Get sprite index into Y.
	lda	(ptr2),y	; Retrieve old counter value
	sub	#1		; Subtract one frame.
	sta	(ptr2),y	; Store new counter value
	bpl	noanim
	;; Now do the animation.
	lda	spridx		; Sprite index into A.
	add	#8		; Eight bytes later is the initial frame skip value.
	tay			; Frame skip index into Y.
	lda	(ptr2),y	; Get initial value.
	ldy	spridx		; Now the sprite index into Y.
	sub	#1		; Subtract one from the value as we use BPL.
	sta	(ptr2),y	; Current value set to initial value.
	tya			; Sprite index into A.
	add	#16		; 16 bytes later are the XOR values.
	tay
	lda	(ptr2),y	; Get XOR value.
	sta	smval		; Self-modifying code value.
	ldy	spridx		; Y=sprite index
	lda	(ptr1),y	; Get sprite pointer at sprite index.
	eor	#$FF		; Self-modifying!
	smval=*-1
	sta	(ptr1),y	; Set the new sprite pointer.
noanim:	dec	spridx
	bpl	l1
	rts
