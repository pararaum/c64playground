
	.include	"t7d/sprite/spriteX2E.i"
	.include	"LAMAlib-macros16.inc"

	.macpack	generic

	.export	sprX2E_update_sprites

	.code
.proc	sprX2E_update_sprites
	ldax	SPRX2E_POINTER_CURRENT_WORKAREA
	stax	posptr
	stax	pos2ptr
	clc
	adcax	#16		; 8 x-pos, 8 y-pos, then velocities.
	stax	velptr
	ldx	#16-1		; Eight sprites (x and y), using BPL for loop.
loop:	lda	$FFFF,x		; Self-modifying get position.
	posptr=*-2
	add	$FFFF,x		; Self-modifying add velocity element.
	velptr=*-2
	sta	$FFFF,x		; Self-modifying store the new position.
	pos2ptr=*-2
	dex
	bpl	loop
	rts
.endproc
