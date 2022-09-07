;;; Example for the Full Screen Vertical Scroller in ASM.

	.macpack	longbranch
	.macpack	generic

	.include	"t7d/copy_chargen.i"
	.include	"t7d/memoryconfig.i"
	.include	"t7d/vic/vicmacros.i"
	.include	"t7d/pseudo/pseudo16.inc"
	.include	"t7d/libt7d.i"
	.include	"t7d/scroller/fsvscroll.i"
	.include	"t7d/kernal.i"

	.import	_long_frame
	.import	_muzak_init00
	.import	_muzak_play

	.exportzp	ptr1

	SCREENPTR0 = $c000
	SCREENPTR1 = $c400
	CHARGENPTR = $d000
	.export	CHARGEN_DESTINATION = $d000
	BORDERCOLORSURROGATE = bordersurrrogate
	;; 	BORDERCOLORSURROGATE = $d020 ; bordersurrrogate

	.segment	"STARTUP"
	jmp	_main
	.segment	"INIT"
	.segment	"ONCE"

	.zeropage
ptr1:	.res	2

	.bss
bordersurrrogate:
	.res	1

	.data
	.byte	"T7D"

	.code
.proc	copy_chargen_inplace
	memoryconfig_kernal
	jmp	copy_chargen
.endproc

.proc	init
	lda	$d011
	and	#%11110111
	sta	$d011
	lda	_long_frame+0
	sta	$d020
	lda	_long_frame+1
	sta	$d021
	jsr	copy_chargen_inplace
	lda	#<SCREENPTR0
	ldx	#>SCREENPTR0
	ldy	#' '
	jsr	fill_1000_bytes
	FSVScroll_Initialise _long_frame+2, 80, SCREENPTR0, SCREENPTR1
	P_loadi	next_line, fsvscroll_copy_next_line_fun
	rts
.endproc

.proc	next_line
	jsr	fsvscroll_draw_next_line_at_bottom
	bne	@s1
	jsr	fsvscroll_reset_scroll_ptrs
@s1:
	rts
.endproc

	.code
irq250:
	asl	$d019
	lda	BORDERCOLORSURROGATE
	pha
	dec	BORDERCOLORSURROGATE
	jsr	_muzak_play
	dec	BORDERCOLORSURROGATE
	jsr	_fsvscroll_update_softscroll_up_1
	pla
	sta	BORDERCOLORSURROGATE
	jmp	EXITIRQ

	.code
_main:	sei
	jsr	_disable_cia_irq
	SetIRQ314Pointer	irq250
	EnableIRQatRasterline	250
	jsr	init
	jsr	_muzak_init00
	SwitchScreenAndChargenAddress 	SCREENPTR0, CHARGENPTR
	cli
mainloop:
	jmp	mainloop
