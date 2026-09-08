	.include	"t7d/sprite/sprite.i"
	.include	"t7d/memoryconfig.i"
	.include	"globals.i"
	.include	"moiety.smoking_hot.i"

	;; Sprites are assumed to be at $C400.
SPRBUF=$C400
SPRX=24+158
SPRY=50-4
SPRCOL=GFX_foreground

	.data
framebit:	.byte	%00001000

	.code
.proc	smoking_hot_init
	lda	#SPRCOL
	jsr	set_all_sprites_colour
	;;  Just position all sprites at the same spot.
	.repeat	8,I
	 positionSpriteAbsolute	I,::SPRX,::SPRY
	.endrepeat
	sei
	memoryconfig_ram
	SetConsecutiveSpriteBufs SPRBUF,GFX_hiscreenaddr+1024-8
	memoryconfig_io
	cli
	lda	#1		; Select one sprite.
	sta	$d015		; And turn it on!
	rts
.endproc

	
.proc	smoking_hot_run
	;; Synthetic ROL, see https://www.nesdev.org/wiki/Synthetic_instructions.
	lda	framebit
	cmp	#$80		; Synthetic ROL
	rol
	sta	framebit
	bpl	out
	lda	$d015
	cmp	#$80
	rol
	sta	$d015
out:
	rts
.endproc

	
