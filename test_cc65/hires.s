;;; ca65 -t c64 -I ../includeCC65/ hires.s && cl65  hires.o

;;;	Displays an Hires image from $E000 and the colour information is at $DC00.

	.include	"vicmacros.i"
	.include	"pseudo16.inc"

	.segment "INIT"
	.segment "ONCE"
	.segment "STARTUP"
	cld
	cld
	jmp	_main
	
	.code
_main:
	lda	#$00
	sta	$d020
	jsr	init
	inc	$d020
	sei
	lda	$1
	pha
	lda	#$30		; All ram configuration.
	sta	$1
	lda	#$56
	ldx	#0
l37:	sta	$dc00,x
	sta	$dd00,x
	sta	$de00,x
	sta	$df00,x
	dex
	bne	l37
	inc	$d020
	;; 
	P_loadi	$2,$e000
	P_loadi $4,$e000+8000
l1:	P_storeb $2,$2
	P_transfer $2,$6
	P_inc	$2
	P_sub	$4,$6
	P_branchNZ $6,l1
	;;
	pla
	sta	$1
	cli
	inc	$d020
	rts


init:
	SwitchVICBank 3
	SetHiresBitmapMode
	SetBitmapAddress $2000
	SetScreenMemory $1c00
	rts
