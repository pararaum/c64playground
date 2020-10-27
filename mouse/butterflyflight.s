
	.include	"kernal.i"
	;; https://wiki.cc65.org/doku.php?id=cc65:using_runtime_zeropage_locations_in_assembly_language&s[]=ptr1
	;; https://github.com/cc65/wiki/wiki/Using-runtime-zeropage-locations-in-assembly-language
	.include	"zeropage.inc"

	.import	game
	.import __MUZAK_RUN__

	.segment	"MUZAK"
	.incbin	"../anichar/Back_to_Basics.sid",$7c+2

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
	.code
_brk:	tsx
	lda	$0105,x
	sta	ptr1
	lda	$0106,x
	sta	ptr1+1
	lda	ptr1
	bne	@skip
	dec	ptr1+1
@skip:	dec	ptr1
	ldy	#0
	lda	($fe),y
	sta	$0400
	jmp	$ea81

	.code
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
	lda	#0
	jsr	__MUZAK_RUN__	; Initialise music
	jsr	title_screen
	jsr	options_screen
	jsr	game
	jsr	consistency_check
	rts
