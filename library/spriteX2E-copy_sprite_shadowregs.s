
	.include	"t7d/sprite/spriteX2E.i"

	.export	sprX2E_copy_sprite_shadowregs

	.proc sprX2E_copy_sprite_shadowregs
	ldy	#0
	ldx	#0
	stx	$d010				    ; Clear all the MSBs.
lx:	lda	(SPRX2E_POINTER_CURRENT_WORKAREA),y ; Get current X value.
	asl					    ; Multiply by two
	sta	$d000,x				    ; Write to X position.
	ror	$d010				    ; Put carry into MSB register, remember eight sprites.
	iny
	inx
	inx
	cpy	#8		; All eight x value copied?
	bne	lx
	;;  Y=8
	ldx	#0		; Reset X to zero.
ly:	lda	(SPRX2E_POINTER_CURRENT_WORKAREA),y ; Get current Y value.
	sta	$d001,x				    ; Y position.
	iny
	inx
	inx
	cpy	#16		; All eight values copied?
	bne	ly
	rts
	.endproc
