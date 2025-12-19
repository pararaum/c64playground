	.include	"LAMAlib.inc"
	.include	"t7d/memoryconfig.i"
	.include	"resident.i"
	.include	"globals.i"
	.include	"zeropage.inc"
	.include	"moiety.frameengine.i"

	.export	_call_animation
	.export	_jumper_static

	;; Generated code:
	.include	"temporary.4--10.jumper_in_orbit_crash.petscii.inc"
	ANIMATIONSCREEN=MEMMAP_gfxarea

;;; Show the crashed Ski Jumper.


	.rodata
nextpartname:	.asciiz	"***None?***.Z"
_jumper_static:	.incbin	"temporary.jumper_in_orbit_crash.petscii.bin",2,2200

	.data
finished:	.byte	0	; Finish demo if Bit 7 is set.


	.code
.proc	_call_animation
	sta	JMPPTR		; LO of jump address.
	asl			; Multiply by two (127 frames should be enough for everybody!).
	clc
	adc	JMPPTR
	adc	#<animation_petscii_jumptable
	sta	JMPPTR
	lda	#0		; If carry then add it.
	adc	#>animation_petscii_jumptable
	sta	JMPPTR+1
	jmp	*
	JMPPTR=*-2
.endproc


	.segment	"EXEHDR"
	.export	__EXEHDR__:absolute=1
	jsr	init
	jsr	Start
	jsr	main
	jmp	nextpart
Start:
	;; Go to C code.

	.segment	"LOWCODE"
	.code
.proc	init
	memoryconfig_kernal
	lda	#0
	sta	$d020
	sta	$d021
	lda	$d011
	and	#%01101111
	sta	$d011
	lda	#$93		; Clear Screen.
	jsr	CHROUT
	sei
	jsr	RESIDENT_set_default_irq
	cli
	lda	#$c8		; Restore defaults
	sta	$d016
	lda	#$1b
	sta	$d011
	rts
.endproc

	.segment	"LOWCODE"
.proc nextpart
	ldax	#nextpartname
	ldy	#RESIDENT_load8000|RESIDENT_pucrunch
	jmp	RESIDENT_load_nextpart
.endproc

	.code
main:
	nop
waitend:
	bit	finished
	bpl	waitend
	rts
