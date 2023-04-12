	.include "t7d/stackmacros.i"
	.include "t7d/memoryfunctions.i"

	.importzp	ptr1, ptr2

.proc	clear_memory_progmem
	GetReturnAddrIntoPointer	ptr2
	PointerAdjustedToStack	ptr2,4
	ldy	#1
	lda	(ptr2),y
	sta	ptr1
	iny
	lda	(ptr2),y
	sta	ptr1+1
	ldy	#4
	lda	(ptr2),y
	tax
	dey
	lda	(ptr2),y
	jmp	clear_memory
.endproc
