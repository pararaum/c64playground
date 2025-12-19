	.include	"t7d/memoryconfig.i"
	.include	"t7d/memoryfunctions.i"
	.include	"t7d/stringfunctions.i"
	.include	"t7d/koala.i"
	.include	"t7d/theatre/consts.i"
	.include	"t7d/stackmacros.i"
	.include	"t7d/libt7d.i"
	.include	"t7d/sprite/sprite.i"
	.include	"t7d/sprite/spritepad.i"
	.include	"t7d/sprite/spritescroller-rol.i"
	.include	"t7d/vic/screen.i"
	.include	"LAMAlib.inc"
	.include	"resident.i"
	.include	"globals.i"
	.include	"zeropage.inc"
	.include	"moiety.frameengine.i"

COLORS=$D800

	.rodata
private:	.incbin	"assets/private.kla",2
after_jump:	.incbin	"assets/skijumper-after-jump.kla",2

	.data
finished:	.byte	0	; Finish demo if Bit 7 is set.
scroller_on:	.byte	$0	; Call scroller update if Bit 7 is set.

	.segment	"ONCE"
	.segment	"EXEHDR"
	.export	__EXEHDR__:absolute=1

	jsr	_disable_cia_irq
	jsr	main
	jmp	*
Start:

	.code
.proc	setup_gfx
	lda	#$3b
	sta	$d011
	lda	$d016
	ora	#$10
	sta	$d016
	lda	#%00001000
	sta	$d018
	rts
.endproc

.proc	show_private
	lda	#%01101111
	and	$d011
	sta	$d011
	jsr	copy_koala_picture_data_bg
	.word	private
	.word	THEATRE_GPHX
	.word	THEATRE_TEXT
	.word	COLORS
	jmp	setup_gfx
.endproc


.proc	show_after
	lda	#%01101111
	and	$d011
	sta	$d011
	jsr	copy_koala_picture_data_bg
	.word	after_jump
	.word	THEATRE_GPHX
	.word	THEATRE_TEXT
	.word	COLORS
	jmp	setup_gfx
.endproc


	.segment	"LOWCODE"
main:
	sei
	CopyFileIntoMemoryDown $800, "resident", 2
	CopyFileIntoMemoryDown $1000, "assets/Commando_Theme_Remix.sid", $7c+2
	lda	#0
	jsr	$1000
	jsr	RESIDENT_set_default_irq
	cli
	lda	#%01101111
	and	$d011
	sta	$d011
	lda	#1
	sta	$d020
	sta	$d021
loop:
	jsr	show_private
	nop
	delay_ms	7000
	nop
	jsr	show_after
	nop
	delay_ms	7000
	nop
	jmp	loop
	rts


