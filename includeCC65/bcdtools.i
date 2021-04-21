;;; -*- mode: asm -*-

;;; Convert a N byte little endian number to a outM byte little endian BCD number.
;;; Input:
;;;	input=pointer to the input number
;;;	work=work area, must be N bytes big
;;;	output=output area, must be outM bytes big
;;;	N=input number of bytes
;;;	outM=output number of bytes
;;; Output:
;;;	output=BCD number (little endian)
;;; Modifies: A, X and work area
;;; Danger! This function will set the decimal flag for a while. Make sure no interrupts happen or deal with it!
;;; [Jim Butterfield, Compute!, 1983, Issue 38, Page 192.]
	.macro	ConvertToBCD	input, work, output, N, outM
	lda	#0		; A=0
	.repeat outM, i
	sta	output+i	; Clear output area.
	.endrepeat
	.repeat N, i
	lda	input+i		; Copy input to work area.
	sta	work+i
	.endrepeat
	ldx	#8*N-1		; Number of bits minus one.
	sed
	@bitloop:
	asl	work
	.repeat N-1, i
	rol	work+1+i
	.endrepeat
	.repeat	outM, i
	lda	output+i
	adc	output+i
	sta	output+i
	.endrepeat
	dex
	bpl	@bitloop
	cld
	.endmacro
