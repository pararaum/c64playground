;;; -*- mode: asm -*-
;;; Macros to ease stack manipulations.

;;; The follwoing is a pair of functions which are dangerous but can
;	help greatly in handling parameters via the stack. The come in
;	pairs:
;
;	* PullStoreStackptr
;	* RetrievePushStackptr

;;; Pull the return address of the stack and store it in a temporary area. Warning! No major labels can be defined as we are using cheap local labels.
;;; Input: -
;;; Output: -
;;; Modifies: A
;;; Data: @tempstackptrl and @tempstrh are cheap local labels which contain the old stack pointer.
.macro	PullStoreStackptr
	pla
	sta	@tempstackptrl
	pla
	sta	@tempstackptrh
.endmacro

;;; Retrieve the return address from the temporary area and push it back on the stack.
;;; Input: -
;;; Output: -
;;; Modifies: A
.macro	RetrievePushStackptr
	lda	#0
	@tempstackptrh = *-1
	pha
	lda	#0
	@tempstackptrl = *-1
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
