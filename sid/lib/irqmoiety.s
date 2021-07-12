;;; Interrupt handling routines.
	.include	"libt7d.i"
	.include	"kernal.i"

	.export	setup_irq

	
	.bss
;;; Pointer to the address of the player routine to be called once per frame.
	;; WARNING! If not set properly it will crash the machine!
PLAYERIRQROUTINE:	.res	2

	.SEGMENT	"ONCE"
;;; Setup the interrupt routine for playing. It will call the PLAYERIRQROUTINE once per frame. The functions is leaved with interrupts still disabled.
;;; Input: A/X=address of the routine
;;; Output: -
;;; Modifies: A,X,Y
setup_irq:
	sta	PLAYERIRQROUTINE ; Store pointer to the player routine.
	stx	PLAYERIRQROUTINE+1
	sei
	lda	#51
	sta	$d012		; Rasterline for IRQ
	lda	$d011		; MSB of rasterline
	and	#$7f
	sta	$d011
	lda	#1
	sta	$d01a		; Enable raster irq
	SetIRQ314Pointer	irq_routine
	jmp	_disable_cia_irq

redirect:
	jmp	(PLAYERIRQROUTINE)

	.code
irq_routine:
	lda	$dd09		; seconds
	pha
	and	#$f
	adc	#$30
	sta	$0400+999
	pla
	lsr
	lsr
	lsr
	lsr
	clc
	adc	#$30
	sta	$0400+998
	lda	$dd0a		; Minutes
	pha
	and	#$f
	adc	#$30
	sta	$0400+996
	pla
	lsr
	lsr
	lsr
	lsr
	clc
	adc	#$30
	sta	$0400+995
	lda	#':'
	sta	$0400+997	; Store the ':'
	inc	$d020
	asl	$d019		; Acknowledge IRQ
	jsr	redirect	; Call the actual player.
	dec	$d020
	jmp	EXITIRQ
