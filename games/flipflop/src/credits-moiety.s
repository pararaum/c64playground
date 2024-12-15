	.include	"zeropage.inc"
	.include	"LAMAlib-macros16.inc"
	.include	"t7d/sprite/sprite.i"
	.import	popax
	.import	popa
	.macpack	generic

	.export	_colour_ram_filler
	.export	_fader_multiplexer

	.define	xposptr ptr1
	.define	yposptr ptr2
	.define	rasterline tmp1
	.define	current_sprite	tmp2

_fader_multiplexer:
	stax	yposptr
	jsr	popax
	stax	xposptr
	lda	#0		; Start with current sprite is sprite zero.
	sta	current_sprite
loop:
	;; Get next Y position.
	ldy	#0
	lda	(yposptr),y
	beq	jump_out
	sta	rasterline	; Store rasterline.
	lda	(xposptr),y	; Get X position.
	tax			; Put X-pos in X...
	sub	#1
	sta	(xposptr),y
	lda	current_sprite
	asl			; Current sprite *2.
	tay			; ...and into Y.
	lda	rasterline
	sta	$d001,y		; Set the sprites Y pos.
	txa			; X-pos in A.
	asl			; X-pos * 2!
	sta	$d000,y		; Set the sprite X pos (Bit 0-7). C = Bit 8.
	ldx	current_sprite	; Now: X=sprite number!
	lda	SPRITE_HIGH_BITS,x
	eor	#$FF		; Inverse to clean...
	and	$d010		; ...sprite MSBs.
	bcc	@no_msb		; Now check carry again.
	ora	SPRITE_HIGH_BITS,x
@no_msb:
	sta	$d010
	inx			; Advance to next sprite
	txa
	and	#7		; There are only eight of them.
	sta	current_sprite
	inc16	xposptr
	inc16	yposptr
	lda	rasterline	; Rasterline where the sprite begins.
rasterloop:
	cmp	$d012		; Is position reached?
	bcs	rasterloop
	jmp	loop
jump_out:
	rts

_colour_ram_filler:
	tax			; Column into X.
	jsr	popa
	.repeat	24,I
	sta	$d800+I*40,x
	.endrepeat
	rts
