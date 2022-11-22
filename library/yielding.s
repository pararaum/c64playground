	.include	"t7d/pseudo/yielding.i"

	.export	call_yielding_function
	.export	call_yield
	.export	yielding_regsave_a
	.export	yielding_regsave_x
	.export	yielding_regsave_y
	.export	yielding_pc_hi
	.export	yielding_pc_lo

	.bss
yielding_regsave_a:	.res	1
yielding_regsave_x:	.res	1
yielding_regsave_y:	.res	1
yielding_pc_hi:	.res	1
yielding_pc_lo:	.res	1

	.code
.proc call_yielding_function
	jsr	actual
	;; Only return here if yielding function ended in a RTS.
	lda	#>(therts-1)	; Call will point to a RTS
	sta	yielding_pc_hi
	lda	#<(therts-1)
	sta	yielding_pc_lo
therts:	rts
actual:	lda	yielding_pc_hi
	pha
	lda	yielding_pc_lo
	pha
	lda	yielding_regsave_a
	ldx	yielding_regsave_x
	ldy	yielding_regsave_y
	rts			; actuall a jump to yielding_pc
.endproc

.proc	call_yield
	sta	yielding_regsave_a
	stx	yielding_regsave_x
	sty	yielding_regsave_y
	pla
	sta	yielding_pc_lo
	pla
	sta	yielding_pc_hi
	;; Do not return to the point where the function called call_yield.
	pla
	pla
	;; Return to the function which called call_yielding_function!
	rts
.endproc

.proc	yielding_end_reached
	lda	yielding_pc_hi
	cmp	#>(call_yielding_function::therts-1)
	bne	out
	lda	yielding_pc_lo
	cmp	#<(call_yielding_function::therts-1)
out:	rts
.endproc
