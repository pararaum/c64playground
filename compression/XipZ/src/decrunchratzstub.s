DECRUNCHTARGET = $F7
DEFAULTDESTINATIONADDR = $33c
ENDADDRESS = $1000

;;; Remember that $58 contains the last byte copied by the basic routine - 256.
SRCPTR = $58			; Stream pointer.
DSTPTR = $26			; Output pointer.
CPYPTR = $28			; Copy source of a match.
OFFPTR = $5a			; Last offset minus one (free after the block move).

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

;;; Decrunch routine for the ratz cruncher. One control byte per token:
;;;   $00      end of stream
;;;   $01-$7F  literal run, b raw bytes follow
;;;   $80-$9F  match of b-$7F bytes (1..32) at the last offset
;;;   $A0-$DF  match of b-$9E bytes (2..65), one byte follows: offset-1
;;;   $E0-$FF  match of b-$DD bytes (3..34), two bytes follow: offset-1 (lo, hi)
;;; Matches are copied forwards, so an offset smaller than the length repeats data.
;;; Input: SRCPTR, DSTPTR.
decrunchdata:
	.org	DECRUNCHTARGET
	.proc	decrunch
next:	ldy	#0
	lda	(SRCPTR),y	; Get the control byte.
	beq	done
	bmi	match
	;; Literal run of A bytes (1..127), copied backwards.
	tax			; Keep the number of bytes safe.
	tay
	inc	SRCPTR		; Skip the control byte.
	bne	@litpre
	inc	SRCPTR+1
@litpre:
	dey			; Y = n-1 .. 0
litcop:	lda	(SRCPTR),y
	sta	(DSTPTR),y
	dey
	bpl	litcop		; Exit when Y wraps to $FF.
	txa
	clc
	adc	SRCPTR
	sta	SRCPTR
	bcc	@srcok
	inc	SRCPTR+1
@srcok:	txa
	clc
	adc	DSTPTR
	sta	DSTPTR
	bcc	next
	inc	DSTPTR+1
	bcs	next		; Always.
done:	jmp	DEFAULTDESTINATIONADDR
	stubjump = *-2
match:	cmp	#$a0
	bcc	rep		; $80-$9F, C=0
	cmp	#$e0
	bcs	far		; $E0-$FF, C=1
	;; $A0-$DF: near match, C=0.
	sbc	#$9d		; A = b-$9E = length 2..65 (C=0 subtracts one more)
	tax
	iny
	lda	(SRCPTR),y	; offset-1
	sta	OFFPTR
	lda	#0
	sta	OFFPTR+1
	lda	#2		; Token size.
	bne	copy		; Always.
far:	sbc	#$dd		; A = b-$DD = length 3..34
	tax
	iny
	lda	(SRCPTR),y
	sta	OFFPTR
	iny
	lda	(SRCPTR),y
	sta	OFFPTR+1
	lda	#3		; Token size.
	bne	copy		; Always.
rep:	sbc	#$7e		; A = b-$7F = length 1..32
	tax
	lda	#1		; Token size.
copy:				; A = token size, X = length.
	clc
	adc	SRCPTR
	sta	SRCPTR
	bcc	@srcok
	inc	SRCPTR+1
@srcok:	clc			; Copy source = destination - (offset-1) - 1.
	lda	DSTPTR
	sbc	OFFPTR
	sta	CPYPTR
	lda	DSTPTR+1
	sbc	OFFPTR+1
	sta	CPYPTR+1
	ldy	#0
mcp:	lda	(CPYPTR),y
	sta	(DSTPTR),y
	iny
	dex
	bne	mcp
	tya			; A = length.
	clc
	adc	DSTPTR
	sta	DSTPTR
	bcc	@dstok
	inc	DSTPTR+1
@dstok:	jmp	next
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
