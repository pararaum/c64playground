
	.include	"t7d/kernal.i"
	;; https://wiki.cc65.org/doku.php?id=cc65:using_runtime_zeropage_locations_in_assembly_language&s[]=ptr1
	;; https://github.com/cc65/wiki/wiki/Using-runtime-zeropage-locations-in-assembly-language
	.include	"zeropage.inc"

	.import	game
	.import __MUZAK_RUN__
	.import __BSS_SIZE__, __BSS_RUN__

	.export	screen0, screen1
	.export	spriteptr0, spriteptr1
	.export	font

	.segment	"MUZAK"
	.incbin	"../anichar/Back_to_Basics.sid",$7c+2

	.segment	"GFX"
screen0:
	.res	1000
	.res	16		; Empty
spriteptr0:
	.res	8
screen1:
	.res	1000
	.res	16		; Empty
spriteptr1:
	.res	8
font:
	.incbin	"scrap_writer_iii_06.64c",2
	.res	$800-(*-font),$22 ; Fill up...
spritedata:
	.res	$3000


	.segment	"STARTUP"
	.word	_main
	.word	_nmi
	.byte	$c3,$c2,$cd,"80"
	sei
	;; Clear BSS, https://github.com/cc65/cc65/blob/master/libsrc/common/zerobss.s
	lda	#<__BSS_RUN__
	sta	ptr1
	lda	#>__BSS_RUN__
	sta	ptr1+1
	lda	#0
	ldy	#0
	ldx	#>__BSS_SIZE__
	beq	@nohigh
@l1:
	sta	(ptr1),y
	dey
	bne	@l1
	inc	ptr1+1
	dex
	bne	@l1
@nohigh:
	cpy	#<__BSS_SIZE__
	beq	@gomain
	sta	(ptr1),y
	iny
	bne	@nohigh
@gomain:
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
