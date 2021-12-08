
	.export	set_all_sprites_colour

set_all_sprites_colour:
	stx	SAVEX		; Keep old X value save.
	ldx	#8-1		; Eight sprites to handle.
l1:	sta	$d027,x		; Set colour.
	dex			; Next sprite.
	bpl	l1		; All done?
	ldx	#0
	SAVEX=*-1		; For self-modifying code.
        rts
