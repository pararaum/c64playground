
	.include	"kernal.i"
	.import	game

	.segment	"STARTUP"
	.word	_main
	.word	_nmi
	.byte	$c3,$c2,$cd,"80"
	sei
	jsr	_main
	brk
	.byte	$56		; Cross marks the spot.
	rts

	.code
_nmi:	rti

;;; BRK handler
;;; http://nesdev.com/the%20'B'%20flag%20&%20BRK%20instruction.txt
_brk:	tsx
	lda	$0105,x
	sta	$fe
	lda	$0106,x
	sta	$ff
	lda	$fe
	bne	@skip
	dec	$ff
@skip:	dec	$fe
	ldy	#0
	lda	($fe),y
	sta	$0400
	jmp	$ea81

init:
	jsr	RESTOR
	jsr	IOINIT
	jsr	SCINIT
	lda	#<_brk
	sta	$316
	lda	#>_brk
	sta	$317
	rts

title_screen:
	rts

options_screen:
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
