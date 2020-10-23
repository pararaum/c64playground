
	.segment	"STARTUP"
	.word	_main
	.word	_nmi
	.byte	$c3,$c2,$cd,"80"
	jsr	_main
	brk
	brk
	rts

	.code
_nmi:	rti

_brk:	jmp	$ea81

init:	lda	#<_brk
	sta	$316
	lda	#>_brk
	sta	$317
	rts

title_screen:
	rts

options_screen:
	rts

game:
	rts

consistency_check:
	rts


	.code
_main:	cld
	jsr	init
	jsr	title_screen
	jsr	options_screen
	jsr	game
	jsr	consistency_check
	rts
