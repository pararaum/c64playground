	.include	"t7d/memoryconfig.i"
	.include	"t7d/vic/vicmacros.i"
	.include	"t7d/libt7d.i"
	.include	"t7d/kernal.i"
	.include	"t7d/memoryfunctions.i"
	.include "LAMAlib-macros16.inc"

	.import	Start
	.import __HIMEM__, __STACKSIZE__
	.import	__FONT_START__
	.import	initlib, zerobss, callmain
	.import	BSOUT
	.importzp	sp, ptr1, ptr2, tmp1, tmp2
	.import	_xorshift_internal_state
	.import muzak_init, muzak_play
	.import	screen_color_data
	.import	credits_screen

	SCREENNBORDER = 0
	FGCONTROL = $9F		; $9c for purple, $df for cyan
	
	.segment	"EXEHDR"
	cld
        lda     #<(__HIMEM__)
        sta     sp
        lda     #>(__HIMEM__)
        sta     sp+1
	lda	#0
	jsr	muzak_init
	jsr	setup_irq
	jsr	setup_vic
	memoryconfig_kernal
	jsr	initlib
	jsr	zerobss
	jsr	callmain
	jmp	64738

.proc	setup_vic
	lda	#SCREENNBORDER
	sta	$d020
	sta	$d021
	lda	#FGCONTROL
	jsr	BSOUT
	lda	#<(screen_color_data)
	sta	ptr1
	lda	#>(screen_color_data)
	sta	ptr1+1
	lda	#<($d800)
	sta	ptr2
	lda	#>($d800)
	sta	ptr2+1
	lda	#<1000
	ldx	#>1000
	jsr	memcpy_up
	ldax	#511
	jsr	wait_frames
	lda	$d011
	and	#$7f
	pha
	lda	#0
	sta	$d011
	lda	#$93		; Clear
	jsr	BSOUT
	SetChargenAddress	__FONT_START__
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
	ldax	#373
	jsr	wait_frames
	rts
.endproc

	.segment	"LOWCODE"

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

