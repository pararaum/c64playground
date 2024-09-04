	.include	"zeropage.inc"
	.include	"t7d/theatre.i"
	.include	"t7d/pseudo/loadstorereg.i"
	.include	"t7d/compression/xipz-qadz.decrunch.inc"
	.include	"t7d/memoryconfig.i"
	.import	popax

	.export	_theatre_copy_compressed_frame

;;; Input: A=frame number, C-stack=staging area
;;; Modifies: A,X,Y tmp1,ptr1,ptr2,ptr3
_theatre_copy_compressed_frame:
	asl			; frame number is multiplied by two
	sta	tmp1		; Keep frame number save in tmp1
	;; Set to all memory memconfig.
	lda	1		; Get old memory config.
	pha
	memoryconfig_ram
	jsr	popax		; Get staging area
	Store16	ptr2,a,x
	ldy	tmp1		; Get framenumber*2 into Y
	lda	(_theatre_universal_pointer),y
	sta	ptr1
	sta	return_compLO
	iny
	lda	(_theatre_universal_pointer),y
	sta	ptr1+1
	sta	return_compHI
	jsr	decrunch_qadz
	;; Restore old memory configuration.
	pla
	sta	1
	lda	#0		; LO of compressed data
	return_compLO=*-1
	ldx	#0		; HI of compressed data
	return_compHI=*-1
	rts
