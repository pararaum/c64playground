; -*- mode: asm -*-

;;; Library for implementing a kind of "yield" as in Python for the 6502.

	;; Storage for A
	.global	yielding_regsave_a
	;; Storage for X
	.global	yielding_regsave_x
	;; Storage for Y
	.global	yielding_regsave_y
	;; Storage for the function addr - 1
	.global	yielding_pc_hi
	;; Storage for the function addr - 1
	.global	yielding_pc_lo

;;; Call the function the storage was prepared for. Warning! Always set yielding_pc_* beforehand or this will crash!
;;; If the end of the yielding function is reached (the final RTS) then the function pointer yielding_pc_* is set to a RTS.
;;; Input: yielding_*
;;; Output: X, Y = value they had within the yielding function
;;; Modifies: A, X, Y, yielding_*
	.global	call_yielding_function

;;; Function to be called from within a yielding function. Return to the calling function and set everything up for the next call.
;;; Warning! Always call from top level not from a further subroutine as the stack is heavily modified.
;;; Warning! Registers are saved, processor status is *not*! Make sure that there is no yield between a comparison and the corresponding branch!
	.global	call_yield

;;; Predicate to check if the yielding function reached its end. Will check if yielding_pc_* points to the final always returning RTS.
;;; Input: -
;;; Output: Z=1 if end reached
;;; Modifies: A
	.global	yielding_end_reached

;;; Setup the yielding function. This will not actually call the function!
;;; Input: function = address of the function
;;; 	acc = set initial value for A
;;; 	xreg = set initial value for X
;;; 	yreg = set initial value for Y
;;; Output: -
;;; Modifies: A
.macro	SetupYielding	function, acc, xreg, yreg
	.ifnblank	acc
	lda	#(acc)
	sta	yielding_regsave_a
	.endif
	.ifnblank	xreg
	lda	#(xreg)
	sta	yielding_regsave_x
	.endif
	.ifnblank	yreg
	lda	#(yreg)
	sta	yielding_regsave_y
	.endif
	lda	#>(function-1)
	sta	yielding_pc_hi
	lda	#<(function-1)
	sta	yielding_pc_lo
.endmacro

;;; Call the yielding function.
;;; Input: yielding_*
;;; Output: yielding_*
;;; Modifies: *
.macro	CallYielding
	jsr	call_yielding_function
.endmacro

;;; Call the yielding function as above but call also the check if end was reached.
;;; Input: yielding_*
;;; Output: yielding_*
;;; Modifies: *
.macro	CallYieldingCheckEnd
	jsr	call_yielding_function
	jsr	yielding_end_reached
.endmacro

;;; Execute the yield. Warning! Can only be called within a yielding function!
;;; Input: yielding_*
;;; Output: yielding_*
;;; Modifies: *
.macro	Yield
	jsr	call_yield
.endmacro
