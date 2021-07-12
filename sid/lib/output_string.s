	.include	"kernal.i"
	.include	"stackmacros.i"
	.importzp	ptr1

	.export	_output_string20
	.export	output_string20
	.export	output_ascii
	.export	output_stringat20

	.segment	"ONCE"

;;; Print a single character ASCII.
output_ascii:
	;; Poor man's conversion of ASCII vs. PETSCII.
	cmp	#$41
	bcc	@no
	EOR	#$20
@no:
	jmp	CHROUT

;;; Output a string which is at most $20 characters long. A zero terminates the string.
;;; Input: A/X=pointer to the string
;;; Output: -
;;; Modifies: A,Y,ptr1
_output_string20:
	sta	ptr1
	stx	ptr1+1
	;; Fall through!
;;; Subfunction which can be called with an initialised ptr1.
;;; Input: ptr1
output_string20:
	ldy	#0
@l1:	lda	(ptr1),y
	beq	@end
	jsr	output_ascii
	iny
	cpy	#$20		; Maximum length reached?
	bne	@l1
@end:
	rts

output_stringat20:
	PullStoreStackptr
	pla			; Get column
	tay
	pla
	tax			; Get row
	clc
	jsr	PLOT
	pla			; Get stringptr HI
	tax
	pla
	jsr	_output_string20
	RetrievePushStackptr
	rts
