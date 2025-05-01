	.include	"t7d/memoryconfig.i"
	.include	"t7d/vic/vicmacros.i"
	.include	"t7d/libt7d.i"
	.include	"t7d/kernal.i"
	.include	"t7d/memoryfunctions.i"
	.include "LAMAlib-macros16.inc"

	.import	__FONT_START__
	.import	BSOUT
	.importzp	sp, ptr1, ptr2, tmp1, tmp2
	.import muzak_init, muzak_play
	.import	credits_screen
	.import	title_font, credits_font, game_font
	.import	SCREEN_AND_BORDER_COL

	FGCONTROL = $9F		; $9c for purple, $df for cyan

	.export	_init_assets
	.export	_init_music_1

	.code
_init_music_1:
	lda	#1
	sei
	jsr	muzak_init
	cli
	rts

	.code
_init_assets:
	cld
	lda	$d011		; Screen off
	and	#%11101111
	sta	$d011
	lda	#0
	jsr	muzak_init
	jsr	setup_irq
	jsr	setup_vic
	jsr	display_title
	lda	$d011		; Screen on
	ora	#%00010000
	sta	$d011
	ldax	#511
	jsr	wait_frames
	jsr	display_credits
	ldax	#373
	jsr	wait_frames
	ldax	#game_font
	stax	ptr1
	ldax	#__FONT_START__
	stax	ptr2
	ldax	#$800
	jsr	memcpy_up
	rts

.proc	display_title
	ldax	#title_font
	stax	ptr1
	ldax	#__FONT_START__
	stax	ptr2
	ldax	#$800
	jsr	memcpy_up
	rts
.endproc

.proc	display_credits
	lda	$d011
	and	#$7f
	pha
	lda	#0
	sta	$d011
	lda	#$93		; Clear
	jsr	BSOUT
	ldax	#credits_font
	stax	ptr1
	ldax	#__FONT_START__
	stax	ptr2
	ldax	#$800
	jsr	memcpy_up
	lda	#<(credits_screen)
	sta	ptr1
	lda	#>(credits_screen)
	sta	ptr1+1
	lda	#<($400)
	sta	ptr2
	lda	#>($400)
	sta	ptr2+1
	lda	#<1000
	ldx	#>1000
	jsr	memcpy_up
	pla
	sta	$d011
	rts
.endproc
	
.proc	setup_vic
	lda	SCREEN_AND_BORDER_COL
	sta	$d020
	sta	$d021
	SetChargenAddress	__FONT_START__
	lda	#FGCONTROL
	jsr	BSOUT
	rts
.endproc

.proc	wait_frames
	sta	tmp1
	stx	tmp2
loop:	jsr	busywait_frame_mp
	jsr	GETIN
	cmp	#0
	bne	out
	lda	tmp1
	beq	underflow
	dec	tmp1
	jmp	loop
underflow:
	dec	tmp1
	dec	tmp2
	bpl	loop
out:	rts
.endproc

.proc	setup_irq
	sei
	SetIRQ314Pointer irq
	jsr	_disable_cia_irq
	EnableIRQatRasterline 250
	cli
	rts
irq:	jsr	muzak_play
	asl	$d019
	jmp	$ea31
.endproc

