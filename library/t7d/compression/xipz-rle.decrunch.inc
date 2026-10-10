; -*- mode: asm -*-

;;; Decrunch routine for the rle cruncher (xipz -a rle -r).
;;;
;;; If the two pointers are not defined before including this file
;;;	then the defaults (ptr1, ptr2) are used.
;;;
;;; Input:
;;;	- XIPZSRCPTR: must point to compressed data
;;;	- XIPZDSTPTR: must point to destination address
;;; Modifies: A, X, Y, XIPZSRCPTR, XIPZDSTPTR
;;; Output: XIPZDSTPTR points behind the last decrunched byte, XIPZSRCPTR
;;;	points to the second byte of the end marker.
;;;
;;; Format: a byte is copied to the output. If the next byte is the
;;;	same, it is skipped and the following byte is a count: that many
;;;	further copies of the byte are written (1..255). A count of zero
;;;	ends the stream.

	.ifndef	XIPZSRCPTR
XIPZSRCPTR = ptr1
	.endif
	.ifndef	XIPZDSTPTR
XIPZDSTPTR = ptr2
	.endif

.proc	decrunch_rle
next:	ldy	#0
	lda	(XIPZSRCPTR),y	; Get the next byte.
	inc	z:XIPZSRCPTR
	bne	@srcok
	inc	z:XIPZSRCPTR+1
@srcok:	cmp	(XIPZSRCPTR),y	; Is the next byte the same?
	beq	run
	sta	(XIPZDSTPTR),y	; No, copy it.
	inc	z:XIPZDSTPTR
	bne	next
	inc	z:XIPZDSTPTR+1
	bne	next		; Always.
run:	iny			; Y=1, a run, get the count.
	lda	(XIPZSRCPTR),y
	beq	done		; Zero, end of stream.
	tax			; X = number of further copies.
	dey
	lda	(XIPZSRCPTR),y	; Get the byte again.
	sta	(XIPZDSTPTR),y	; The first copy.
	inc	z:XIPZDSTPTR
	bne	fill
	inc	z:XIPZDSTPTR+1
fill:	sta	(XIPZDSTPTR),y
	iny
	dex
	bne	fill
	tya			; A = count, 1..255.
	clc
	adc	z:XIPZDSTPTR
	sta	z:XIPZDSTPTR
	bcc	@dstok
	inc	z:XIPZDSTPTR+1
@dstok:	lda	#2		; Skip the repeated byte and the count.
	clc
	adc	z:XIPZSRCPTR
	sta	z:XIPZSRCPTR
	bcc	next
	inc	z:XIPZSRCPTR+1
	bne	next		; Always.
done:	rts
.endproc
