DECRUNCHTARGET = $F7
DEFAULTDESTINATIONADDR = $33c
ENDADDRESS = $1000

;;; Remember that $58 contains the last byte copied by the basic routine - 256.
SRCPTR = $58			; Stream pointer.
DSTPTR = $26			; Output pointer.

	.include	"t7d/basic.i"
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
	.export	stubpageLO=*-$801
	.export	stubpageHI=*+1-$801
	.word	ENDADDRESS
	.export stubendofcdata_offset=*-$801
	.word	tocopy_end
	.byte	"t7d"
	.export stubbeginofcdata_offset=*-$801
	.word	tocopy
parameters_end:
	.export	stubparameters_offset=parameters-$801

;;; Decrunch routine for the rle cruncher. A byte is copied to the output. If
;;; the next byte is the same, it is skipped and the following byte is a
;;; count: that many further copies of the byte are written. A count of
;;; zero is the end of the stream.
;;; Input: SRCPTR, DSTPTR.
decrunchdata:
	.org	DECRUNCHTARGET
	.proc	decrunch
next:	ldy	#0
	lda	(SRCPTR),y	; Get the next byte.
	inc	SRCPTR
	bne	@srcok
	inc	SRCPTR+1
@srcok:	cmp	(SRCPTR),y	; Is the next byte the same?
	beq	run
	sta	(DSTPTR),y	; No, copy it.
	inc	DSTPTR
	bne	next
	inc	DSTPTR+1
	bne	next		; Always.
run:	iny			; Y=1, a run, get the count.
	lda	(SRCPTR),y
	beq	done		; Zero, end of stream.
	tax			; X = number of further copies.
	dey
	lda	(SRCPTR),y	; Get the byte again.
	sta	(DSTPTR),y	; The first copy.
	inc	DSTPTR
	bne	fill
	inc	DSTPTR+1
fill:	sta	(DSTPTR),y
	iny
	dex
	bne	fill
	tya			; A = count, 1..255.
	clc
	adc	DSTPTR
	sta	DSTPTR
	bcc	@dstok
	inc	DSTPTR+1
@dstok:	lda	#2		; Skip the repeated byte and the count.
	clc
	adc	SRCPTR
	sta	SRCPTR
	bcc	next
	inc	SRCPTR+1
	bne	next		; Always.
done:	jmp	DEFAULTDESTINATIONADDR
	stubjump = *-2
	.endproc
	.reloc
decrunchdata_end:

	.export	stubjump_offset = decrunch::stubjump-DECRUNCHTARGET+decrunchdata-$801

	.code
_main:				; Must be the first code so that SYS works.
	;; Copy the copy parameters.
	ldx	#parameters_end-parameters-1
@pl:
	lda	parameters,x	; Get the three parameters.
	sta	z:$58,x		; Store them in the ZP.
	dex
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
	.export	stubdestination_offsetLO=*-1-$801
	sta	DSTPTR
	lda	#>DEFAULTDESTINATIONADDR
	.export	stubdestination_offsetHI=*-1-$801
	sta	DSTPTR+1
	jmp	decrunch
