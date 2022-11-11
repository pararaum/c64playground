	.include	"t7d/controlfunctions.i"
	.include	"t7d/stackmacros.i"

	.importzp	ptr1, ptr2

	.export	run_function_list

;;; Modifies: A, X, Y, ptr1, ptr2, byte after JSR
	.code
	.proc	run_function_list
	GetReturnAddrIntoPointer	ptr1
	PointerAdjustedToStack	ptr1, 4
	ldy	#1
	lda	(ptr1),y	; current index
	tax			; store current index in X
	iny
	;; If equal to the maximum value go to the RTS and Z=1 in this case.
	cmp	(ptr1),y	; maximum index
	beq	out		; Z=1
	iny
	lda	(ptr1),y	; LO of table address
	sta	ptr2
	iny
	lda	(ptr1),y	; HI of table address
	sta	ptr2+1
	txa			; index to offset in table
	asl
	tay
	lda	(ptr2),y	; LO of function pointer
	sta	jsrptr
	iny
	lda	(ptr2),y	; HI of function pointer
	sta	jsrptr+1
	jsr	jsrcmd
	bcc	out
	ldy	#1		; current index
	lda	(ptr1),y	; retrieve current index
	clc
	adc	#1		; next index
	sta	(ptr1),y	; store incremented index
	;; Z=0 as ADC results in a non-zero value.
out:	rts
	;; Store ptr1 as it may be changed in the function.
jsrcmd:	lda	ptr1
	pha
	lda	ptr1+1
	pha
	jsr	jsrptr
jsrptr=*-2
	;; Now retrieve the value of ptr1.
	pla
	sta	ptr1+1
	pla
	sta	ptr1
	rts
	.endproc
