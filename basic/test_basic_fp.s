;;; cl65 --asm-include-dir ../library -t c64 test_basic_fp.s
	.include	"t7d/basic.i"

	.segment	"STARTUP"
	jmp	main
	.segment	"INIT"
	.segment	"ONCE"
	.code
print:	
	jsr	BASIC_FLOAT_OUT
	jmp	STROUT

main:	Fac1LoadFromByte	-1
	jsr	print
	Fac1LoadFromByte	127
	jsr	print
	Fac1LoadFromByte	255
	jsr	print
	Fac1LoadFromByte	-128
	jsr	print
	rts
