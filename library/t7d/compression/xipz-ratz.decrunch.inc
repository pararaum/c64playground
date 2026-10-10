; -*- mode: asm -*-

;;; Decrunch routine for the ratz cruncher (xipz -a ratz -r).
;;;
;;; If the three pointers are not defined before including this file
;;;	then the defaults (ptr1, ptr2, ptr3) are used.
;;;
;;; Input:
;;;	- XIPZSRCPTR: must point to compressed data
;;;	- XIPZDSTPTR: must point to destination address
;;; Modifies: A, X, Y, XIPZSRCPTR, XIPZDSTPTR, XIPZAUXPTR (copy source of a match)
;;; Output: XIPZDSTPTR points behind the last decrunched byte, XIPZSRCPTR
;;;	points to the end token of the stream.
;;;
;;; Format: one control byte b per token, the stream ends with $00.
;;;	$01-$7F: literal run, b raw bytes follow
;;;	$80-$9F: match of b-$7F bytes (1..32) at the last offset
;;;	$A0-$DF: match of b-$9E bytes (2..65), one byte follows: offset-1
;;;	$E0-$FF: match of b-$DD bytes (3..34), two bytes follow: offset-1 (lo, hi)
;;; Matches are copied forwards, so an offset smaller than the length
;;; repeats data. The last offset is kept inside the routine, therefore
;;; the routine must be located in RAM.

	.ifndef	XIPZSRCPTR
XIPZSRCPTR = ptr1
	.endif
	.ifndef	XIPZDSTPTR
XIPZDSTPTR = ptr2
	.endif
	.ifndef	XIPZAUXPTR
XIPZAUXPTR = ptr3
	.endif

.proc	decrunch_ratz
next:	ldy	#0
	lda	(XIPZSRCPTR),y	; Get the control byte.
	beq	done
	bmi	match
	;; Literal run of A bytes (1..127), copied backwards.
	tax			; Keep the number of bytes safe.
	tay
	inc	z:XIPZSRCPTR	; Skip the control byte.
	bne	@litpre
	inc	z:XIPZSRCPTR+1
@litpre:
	dey			; Y = n-1 .. 0
litcop:	lda	(XIPZSRCPTR),y
	sta	(XIPZDSTPTR),y
	dey
	bpl	litcop		; Exit when Y wraps to $FF.
	txa
	clc
	adc	z:XIPZSRCPTR
	sta	z:XIPZSRCPTR
	bcc	@srcok
	inc	z:XIPZSRCPTR+1
@srcok:	txa
	clc
	adc	z:XIPZDSTPTR
	sta	z:XIPZDSTPTR
	bcc	next
	inc	z:XIPZDSTPTR+1
	bcs	next		; Always.
done:	rts
match:	cmp	#$a0
	bcc	rep		; $80-$9F, C=0
	cmp	#$e0
	bcs	far		; $E0-$FF, C=1
	;; $A0-$DF: near match, C=0.
	sbc	#$9d		; A = b-$9E = length 2..65 (C=0 subtracts one more)
	tax
	iny
	lda	(XIPZSRCPTR),y	; offset-1
	sta	lastoff
	lda	#0
	sta	lastoff+1
	lda	#2		; Token size.
	bne	copy		; Always.
far:	sbc	#$dd		; A = b-$DD = length 3..34
	tax
	iny
	lda	(XIPZSRCPTR),y
	sta	lastoff
	iny
	lda	(XIPZSRCPTR),y
	sta	lastoff+1
	lda	#3		; Token size.
	bne	copy		; Always.
rep:	sbc	#$7e		; A = b-$7F = length 1..32
	tax
	lda	#1		; Token size.
copy:				; A = token size, X = length.
	clc
	adc	z:XIPZSRCPTR
	sta	z:XIPZSRCPTR
	bcc	@srcok
	inc	z:XIPZSRCPTR+1
@srcok:	clc			; Copy source = destination - (offset-1) - 1.
	lda	z:XIPZDSTPTR
	sbc	lastoff
	sta	z:XIPZAUXPTR
	lda	z:XIPZDSTPTR+1
	sbc	lastoff+1
	sta	z:XIPZAUXPTR+1
	ldy	#0
mcp:	lda	(XIPZAUXPTR),y
	sta	(XIPZDSTPTR),y
	iny
	dex
	bne	mcp
	tya			; A = length.
	clc
	adc	z:XIPZDSTPTR
	sta	z:XIPZDSTPTR
	bcc	@dstok
	inc	z:XIPZDSTPTR+1
@dstok:	jmp	next
lastoff:	.word	0	; Last offset minus one.
.endproc
