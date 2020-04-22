
	.export	_main
	.export SCREENPTR
	.export wavymation_chargenptr
	.export wavymation_screenptr
	.import fillcolram
	.import	wavymation_copy_font
	.import wavymation_animate_font
	.import wavymation_generate_image

	RASTERIRQROW = 250
	SKIPFRAMESANIM = 4
	SCREENPTR = $c000
	CHARGENPTR = $c800
	wavymation_chargenptr = CHARGENPTR
	wavymation_screenptr = SCREENPTR

	.code
	.include "screen_n_char.inc"

	.rodata
muzak:	.incbin	"Waitress_on_Roller_Skates.sid",$7c+2
logo:
	.incbin	"logo.t7d.40x25.pbm",9
sieve:
	.incbin	"image.40x25.sieve.pbm",9

	.data
framecounter:	.byte	0

	.macro	wavycall	img
	lda	#<img
	ldx	#>img
	jsr	wavymation_generate_image
	jsr	waitframe
	jsr	waitframe
	jsr	waitframe
	.endmacro

	.data
old_irq_isr:	.word	0
	.code
interrupt_service_routine:
	dec	framecounter
	dec	@anmctr
	bpl	@noanim
	jsr	wavymation_animate_font
	lda	#SKIPFRAMESANIM-1
	sta	@anmctr
@noanim:
	jsr	muzak+3
	asl	$d019		; Acknowledge interrupt...
	jmp	(old_irq_isr)
@anmctr: .byte 0
	.code
waitframe:
	lda	#509/3
	sta	framecounter
@l:	lda	framecounter
	bne	@l
	rts


setup_irq:
	lda	$314
	sta	old_irq_isr
	lda	$315
	sta	old_irq_isr+1
	lda	#<interrupt_service_routine
	sta	$314
	lda	#>interrupt_service_routine
	sta	$315
	;; Disable CIA timer interrupts.
        lda     #$7f
        sta     $dc0d
        sta     $dd0d
	;; Clear pending interrupts.
        lda     $dc0d
        lda     $dd0d
        lda     #$7f
        and     $d011           ; Set MSB of raster to zero
        sta     $d011
        lda     #RASTERIRQROW
        sta     $d012
        ;; http://unusedino.de/ec64/technical/project64/mapping_c64.html
        ; 53274         $D01A          IRQMASK, enable raster interrupt.
        lda     #%00000001
        sta     $d01a
        rts


	.code
_main:
	lda	#5
	sta	$d020
	sta	$d021
	lda	#3
	jsr	fillcolram
	jsr	initialise_screenptr_n_chargen
	jsr	wavymation_copy_font
	lda	#0
	jsr	muzak
	sei
	jsr	setup_irq
	cli
mainloop:
	wavycall	logo
	wavycall	sieve
	jmp	mainloop
	brk

