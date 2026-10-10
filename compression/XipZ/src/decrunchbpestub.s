DECRUNCHTARGET = $0100		; Decoder lives in the stack page.
LEFTTABLE = $0400		; Global tables in the text screen.
RIGHTTABLE = LEFTTABLE+$100
DEFAULTDESTINATIONADDR = $33c
ENDADDRESS = $1000

;;; Remember that $58 contains the last byte copied by the basic routine - 256.
SRCPTR = $58			; Stream pointer.
DSTPTR = $26			; Output pointer.
ESCBYTE = $FB			; Escape byte of the stream.
ENDBYTE = $FC			; Escaped byte which ends the stream.

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

;;; Decrunch routine for the bpe cruncher (byte pair encoding). The
;;; stream starts with the escape byte, the end byte and the pair
;;; table: 32 bitmap bytes, each followed by the (left, right) bytes for
;;; the codes marked in it. Every other byte is a token which is
;;; expanded via the tables: a literal
;;; token is its own left and right entry. The escape byte is
;;; followed by a raw byte, if this is the end byte the stream ends.
;;; The expansion uses the hardware stack, the decoder keeps the
;;; escape byte as a sentinel on the stack.
;;; Input: SRCPTR, DSTPTR.
decrunchdata:
	.org	DECRUNCHTARGET
	.proc	decrunch
	ldx	#$FF
	txs
	inx			; X=0.
@init:	txa			; Every token is a literal.
	sta	LEFTTABLE,x
	sta	RIGHTTABLE,x
	inx
	bne	@init
	ldy	#0		; Y=0 from now on.
	jsr	getbyte
	sta	ESCBYTE
	pha			; Sentinel.
	jsr	getbyte
	sta	ENDBYTE
	ldx	#0		; X is the code, counts up to 256.
@bmap:	jsr	getbyte		; Bitmap byte.
	sec
	ror	a		; C = bit 0, a one marks the end.
@nextbit:	bcc	@skip
	pha
	jsr	getbyte
	sta	LEFTTABLE,x
	jsr	getbyte
	sta	RIGHTTABLE,x
	pla
@skip:	inx
	lsr	a
	bne	@nextbit
	txa
	bne	@bmap		; X=0 after 256 codes.
main:	jsr	getbyte
	cmp	ESCBYTE
	beq	esc
expand:	tax
	cmp	RIGHTTABLE,x	; Literal?
	beq	leaf
	lda	RIGHTTABLE,x
	pha			; Right half later.
	lda	LEFTTABLE,x	; Left half now.
	jmp	expand
leaf:	jsr	put
	pla
	cmp	ESCBYTE		; Sentinel?
	bne	expand
	pha
	beq	main		; Always.
esc:	jsr	getbyte
	cmp	ENDBYTE
	beq	done
	jsr	put
	jmp	main
getbyte:
	lda	(SRCPTR),y
	inc	SRCPTR
	bne	:+
	inc	SRCPTR+1
:	rts
put:	sta	(DSTPTR),y
	inc	DSTPTR
	bne	:+
	inc	DSTPTR+1
:	rts
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
	sei
	ldx	#decrunchdata_end-decrunchdata	; Less than 256 bytes.
@cplp:	lda	decrunchdata-1,x
	sta	a:DECRUNCHTARGET-1,x
	dex
	bne	@cplp
	inc	SRCPTR+1	; Adjust to beginning of compressed data.
	lda	#<DEFAULTDESTINATIONADDR
	.export	stubdestination_offsetLO=*-1-$801
	sta	DSTPTR
	lda	#>DEFAULTDESTINATIONADDR
	.export	stubdestination_offsetHI=*-1-$801
	sta	DSTPTR+1
	jmp	decrunch
