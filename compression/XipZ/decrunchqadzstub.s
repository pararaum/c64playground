
DECRUNCHTARGET = $F7
DEFAULTDESTINATIONADDR = $33c
ENDADDRESS = $1000

;;; Remember that $58 contains the last byte copied by the basic routine - 256.
SRCPTR = $58
DSTPTR = $26
AUXPTR = $28

	.include	"basic.i"
	.export	decrunch
	.export	tocopy
	.export	tocopy_end

	.segment	"EXEHDR"
	.word	thebrk
	.word	770
	.byte	$9e,"2061"
thebrk:	brk
	brk
	brk

	.data
tocopy:
	;; 	.byte	"copy"
tocopy_end:

	.rodata
parameters:
	.word	ENDADDRESS
	.word	tocopy_end
	.byte	"t7d"
	.word	tocopy
parameters_end:

;;; Decrunch routine for the qadz cruncher.
;;; Input: SRCPTR, DSTPTR, Y=0
decrunchdata:
	.org	DECRUNCHTARGET
	.proc	decrunch
	ldy	#0
	lda	(SRCPTR),y	; Get next byte.
	bmi	backref		; Back reference and copy.
	bne	literal		; This is a literal run
	jmp	DEFAULTDESTINATIONADDR
literal:
	tax			; Keep number of bytes safe.
	tay			; Put number of bytes into index register.
	inc	SRCPTR		; increment src
	bne	litcop
	inc	SRCPTR+1
litcop:	lda	(SRCPTR),y
	sta	(DSTPTR),y
	dey
	bpl	litcop
	jsr	incsrc
	jsr	incdst
	jmp	decrunch
backref:
	eor	#$ff		; Negate A
	clc
	adc	#1
	tax			; Keep run length safe.
	iny
	lda	(SRCPTR),y	; How far to go back?
	sta	@sbc		; Self-modifying trick.
	lda	DSTPTR		; Subtract from DSTPTR and put into AUXPTR
	sec
	sbc	#00
	@sbc = *-1
	sta	AUXPTR
	lda	DSTPTR+1
	sbc	#0
	sta	AUXPTR+1
	txa
	tay			; Y=X, run length
bckcop:	lda	(AUXPTR),y
	sta	(DSTPTR),y
	dey
	bpl	bckcop
	jsr	incdst		; Adjust destination.
	ldx	#2
	jsr	incsrc		; Skip two bytes
	jmp	decrunch
incsrc:				; increment by X
	txa
	clc
	adc	SRCPTR
	sta	SRCPTR
	lda	SRCPTR+1
	adc	#0
	sta	SRCPTR+1
	rts
incdst:				; increment by X
	txa
	clc
	adc	DSTPTR
	sta	DSTPTR
	lda	DSTPTR+1
	adc	#0
	sta	DSTPTR+1
	rts
	.endproc
	.reloc
decrunchdata_end:

	.code
_main:				; Must be the first code so that SYS works.
	;; Copy the copy parameters.
	ldx	#parameters_end-parameters-1
@pl:
	lda	parameters,x	; Get the three parameters.
	sta	z:$58,x		; Store them in the ZP.
	dex			; Next one.
	bpl	@pl		; parameter loop
	jsr	MEMORY_MOVE	; Leaves with X=0
	;; Now $58/$59 points to beginning of data-256!
	;; X=0, Y=0
	sei
@cplp:	lda	decrunchdata,y
	sta	a:DECRUNCHTARGET,y
	iny
	bne	@cplp
	;; Y=0 here!
	inc	SRCPTR+1	; Adjust to beginning of compressed data.
	lda	#<DEFAULTDESTINATIONADDR
	sta	DSTPTR
	lda	#>DEFAULTDESTINATIONADDR
	sta	DSTPTR+1
	jmp	decrunch
