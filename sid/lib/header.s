
	.import	__MAIN_START__
	.import	_start

	.exportzp	ptr1, ptr2

	.zeropage
	;; Generic pointer.
ptr1:	.res	2
ptr2:	.res	2

	.segment	"LOADADDR"
	.word	__MAIN_START__

	.segment	"EXEHDR"
	jmp	_start

