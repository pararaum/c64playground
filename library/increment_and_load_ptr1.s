	.include	"t7d/pseudo/pointer.i"
	.export	increment_and_load_ptr1
	.export	increment_and_y_load_ptr1

	.importzp	ptr1

increment_and_y_load_ptr1:
	;; We do not use the second macro, in order to save bytes.
	ldy	#0
increment_and_load_ptr1:
	IncrementAndLoad	ptr1
	rts
