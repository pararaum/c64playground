	.include	"t7d/libt7d.i"
	.include	"t7d/koala.i"
	.include	"t7d/kernal.i"
	.include	"t7d/vic/vicmacros.i"
	.include	"t7d/sprite/sprite.i"
	.include	"t7d/sprite/spritetext.i"
	.include	"t7d/memoryfunctions.i"
	.include	"t7d/pseudo/loadstorereg.i"
	.include	"t7d/pseudo/yielding.i"

	.macpack	cbm

	.importzp	ptr1

	TEXTSCREEN = $c800
	SPRITEBUF = $c000
	SPRITECOL = $0
	SPRITELINEWIDTH = 20
	FALLING_COUNTER_START = 80 ; Start value to initialise Falling counter waiting time.
	FALLING_COUNTER_CMP = FALLING_COUNTER_START/2 ; Make colour effect at this falling counter value.

	.export	SPRITETEXT_CHARGEN

	.bss
falling_spritepos:	.res	1 ; Y-position of the falling sprite
falling_counter:	.res	1 ; Counter for wait before sprite falls again

	.data
running:	.byte	0	; if zero then interrupt is busy!

	.rodata
sprcol_tab:
	.byte	0, 9, 2, 8, 10, 7, 1, 7, 10, 8, 2, 9, 0
sprcol_tab_end:
	.data
sprcol_pos:	.byte	0	; position into sprite color table
sprcol_rot:	.byte	1	; rotating byte


	.code
.proc	adjust_sprite_color
	lda	sprcol_rot
	cmp	#$80
	rol
	sta	sprcol_rot
	bpl	out
	inc	sprcol_pos
	ldx	sprcol_pos
	cpx	#sprcol_tab_end-sprcol_tab
	bne	out
	ldx	#0
	stx	sprcol_pos
	sec
	rts
out:
	lda	sprcol_tab,x
	jsr	set_all_sprites_colour
	clc
	rts
.endproc

	.rodata
koala:
	.incbin	"hammurabi.kla",2
	;; 	.incbin	"hammurabi-title.kla",2
koala_end:

hammurabi:
	.byte	$70,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$6e
	.byte	$5d
	scrcode	"     Hammurabi    "
	.byte	$5d
	.byte	$6d,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$7d
	;;	"01234567890abcdef0123456"
	;; 20 characters in double size...
	;;	"01234567890abcdef0123"
credits:
	scrcode	"     Mzk: Chrabz    "
	scrcode	"    Gfx: Logiker    "
	scrcode "    Gfx: ComSha     "
	scrcode	"   Font: DamienG    "
	scrcode	"   Code: Pararaum   "
	scrcode	"     Test: Wil      "

	;; Yes, this is redundant, as the font is loaded later on anyways...
SPRITETEXT_CHARGEN:
	.incbin	"Flamboyant.both.64c",2

	.segment	"EXEHDR"
	jmp	entry

.proc	clear_spritebuf
	Load16	a, y, #SPRITEBUF
	Store16	ptr1, a, y
	Load16	a, x, #8*64
	jmp	clear_memory
.endproc

.proc	irq
	jsr	call_yielding_function
	jmp	*
	oldirq = *-2
.endproc

.proc	init_irq
	SetupYielding	irqbusybee
	Load16	a,x,$314
	sei
	Store16	irq::oldirq,a,x
	SetIRQ314Pointer	irq
	cli
	rts
.endproc

.proc	init_sprites
	jsr	clear_spritebuf
	ldx	#$ff
	stx	$d017		; Double height
	stx	$d01d		; Double width
	stx	$d015		; Enable all
	inx			; X=0
	stx	$d01b		; Sprite is in front of characters.
l1:	txa
	sta	TEXTSCREEN+1024-8,x	; Sprite buffer pointer.
	inx
	cpx	#8
	bne	l1
	;; (/ (- 320 (* 8 24)) 2)64
	.repeat	8,I
	 ;; 	 positionSpriteAbsolute	I, 24+64+24*I, 52
	 positionSpriteAbsolute	I, 24+48*I, 2 ; Outside visible area
	.endrepeat
	lda	#SPRITECOL
	jsr	set_all_sprites_colour
	.repeat	3,I
	 Load16	a,x,#SPRITEBUF+I*3*8
	 Store16	SPRITETEXT_DESTINATION,a,x
	 Load16	a,x,#hammurabi+I*SPRITELINEWIDTH
	 jsr	print_line_to_sprite
	.endrepeat
	rts
.endproc

.proc sprite_falls
	lda	falling_counter
	beq	s2
	dec	falling_counter
	clc
	rts
s2:
	ldy	falling_spritepos
	iny
	cpy	#60
	bcs	nos1
	iny
nos1:
	cpy	#80
	bcs	nos2
	iny
nos2:
	cpy	#100
	bcs	nos3
	iny
nos3:
	cpy	#120
	bcs	nos4
	iny
nos4:
	cpy	#131		; Check for center
	bne	s3
	lda	#FALLING_COUNTER_START
	sta	falling_counter
s3:
	cpy	#145
	bcc	nos5
	iny
nos5:
	cpy	#170
	bcc	nos6
	iny
nos6:
	cpy	#210
	bcc	nos7
	iny
nos7:
	sty	falling_spritepos
	tya
	ldx	#0
l1:	sta	$d001,x		; Y-Pos of sprite
	inx
	inx
	cpx	#16
	bne	l1
	cmp	#251
	rts
.endproc


	.macro	Yieldwait	num
	.local	@loop
	ldx	#num
@loop:
	jsr	call_yield
	dex
	bne	@loop
	.endmacro
.proc	irqbusybee
	Yieldwait	70
	.repeat	5,I
	 lda	#2
	 sta	falling_spritepos
	 lda	#0
	 sta	falling_counter
	 .scope
flp:	 jsr	call_yield
	 lda	falling_counter
	 cmp	#FALLING_COUNTER_CMP
	 bne	no_col_effect
col_effect_loop:
	 jsr	call_yield
	 jsr	adjust_sprite_color
	 bcc	col_effect_loop
no_col_effect:
	 jsr	sprite_falls
	 bcc	flp
	 .endscope
	 Yieldwait	50
	 jsr	clear_spritebuf
	 Load16	a,x,#SPRITEBUF+3*8
	 Store16	SPRITETEXT_DESTINATION,a,x
	 Load16	a,x,#credits+I*20
	 jsr	print_line_to_sprite
	.endrepeat
	Yieldwait	70
	dec	running		; Title Over!
	rts
.endproc
	
	.code
entry:	jsr	copy_koala_picture_data
	.word	koala
	.word	$e000
	.word	TEXTSCREEN
	.word	$d800
	lda	koala_end-1
	sta	$d020
	sta	$d021
	;; 	SwitchVICBank	3
	;; 	lda	#0
	;; 	sta	$dd00
	SetBitmapAddress $2000
	SetScreenMemory TEXTSCREEN
	lda	#%00111011	; Set VIC to bitmap screen.
	sta	$d011
	lda	$d016
	ora	#$10
	sta	$d016
	jsr	init_sprites
	jsr	init_irq
wait:	jsr	GETIN
	cmp	#' '
	beq	ending
	cmp	#3		; RUN/STOP
	beq	ending
	bit	running
	bpl	wait
ending:
	Load16	a,x,irq::oldirq
	sei
	Store16	$314,a,x
	cli
	lda	#0
	sta	$d015		; Disable all sprites.
	rts
