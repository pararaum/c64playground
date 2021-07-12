;;; -*- mode: asm -*-
;;; Macros to ease stack manipulations.

;;; Counter to be used for local labels.

	.ifndef	LOCALLABELCOUNTER
	LOCALLABELCOUNTER .set	0
	.endif

;;; The follwoing is a pair of functions which are dangerous but can
;	help greatly in handling parameters via the stack. The come in
;	pairs:
;
;	* PullStoreStackptr
;	* RetrievePushStackptr

;;; Pull the return address of the stack and store it in a temporary area. Warning! If not used properly this will lead to hard to debug errors!
;;; Input: -
;;; Output: -
;;; Modifies: A
;;; Data: A unique label "UNIQUELABEL%04X"+offset is used to store the old stack pointer.
.macro	PullStoreStackptr
	pla
	sta	.ident(.sprintf("UNIQUELABEL%04X", LOCALLABELCOUNTER))+4
	pla
	sta	.ident(.sprintf("UNIQUELABEL%04X", LOCALLABELCOUNTER))+1
.endmacro

.macro	PullStoreStackptrLOCAL
	pla
	sta	@tempstackptrLO
	pla
	sta	@tempstackptrHI
.endmacro

;;; Retrieve the return address from the temporary area and push it back on the stack.
;;; Input: -
;;; Output: -
;;; Modifies: A
;;; After this macro the LOCALLABELCOUNTER is incremented!
.macro	RetrievePushStackptr
	.ident (.sprintf("UNIQUELABEL%04X", LOCALLABELCOUNTER)):
	lda	#0
	pha
	lda	#0
	pha
	LOCALLABELCOUNTER .set LOCALLABELCOUNTER+1
.endmacro
.macro	RetrievePushStackptrLOCAL
	lda	#0
	@tempstackptrHI=*-1
	pha
	lda	#0
	@tempstackptrLO=*-1
	pha
.endmacro

;;; Adjust the stack pointer.
;;; Input: -
;;; Output: A=new stack pointer value
;;; Modifies: A, X

.macro AdjustStackptr delta
	tsx			; Get current stack pointer.
	txa			; Move stack pointer to A.
	clc
	adc	#<(delta)	; Add value in a to stack pointer. For the use of <(…), see https://github.com/cc65/cc65/issues/186 or https://sourceforge.net/p/cc65/mailman/message/32234634/.
	tax			; X=A as the stack pointer can be transfered to X only.
	txs			; Change the stack pointer to the new value.
.endmacro

;;; Adjust the stack pointer via A.
;;; Input: A=delta to add onto the stack pointer
;;; Output: A=new stack pointer value
;;; Modifies: A, X
	.macro AdjustStackptrA
	.local	@tmp
	sta	@tmp
	tsx			; Get current stack pointer.
	txa			; Move stack pointer to A.
	clc
	adc	#$00		; value in a to stack pointer (via self-modifying code).
	@tmp = *-1
	tax			; X=A as the stack pointer can be transfered to X only.
	txs			; Change the stack pointer to the new value.
	.endmacro
