
;;; Source (compressed data) pointer
LZ4_SRC_PTR	=	$55
;;; End of source data (byte after the end)
LZ4_SRCEND_PTR	=	$57
;;; Destination pointer
LZ4_DST_PTR	=	$59
;;; Storage for repeat or copy count
LZ4_REPEAT_COUNTER16	=	$5b
;;; Storage for the delta offset for match copying
LZ4_DELTA_COPYOP16	=	$5d
;;; Adress to store something while uncrunching. If this is zero, no output.
LZ4_WORKING_BEE = $0403

dstpos	=	$1fa9-2
dstsiz	=	76
TEMPDATAAREA = $c000

	.word	$0801
	*=$0801

	.word	end_of_basic
	.word	main
	.byte	$9e,$20,"2067"
end_of_basic:
	.byte	0,0,0
	.dsb	2067-*,$ea

main:	sei
	lda	#$30		; Only RAM
	sta	$1
	jsr	copy_data_temparea
	;; More information about the format is here https://github.com/lz4/lz4/blob/master/doc/lz4_Block_format.md.
	;; Point LZ4_SRC_PTR to the beginning of the data.
	lda	#<(TEMPDATAAREA+11)
	sta	LZ4_SRC_PTR
	lda	#>(TEMPDATAAREA+11)
	sta	LZ4_SRC_PTR+1
	;; Point LZ4_SRCEND_PTR to the end of the packed data.
	lda	#<(TEMPDATAAREA+pakend-pakdata-4)
	sta	LZ4_SRCEND_PTR
	lda	#>(TEMPDATAAREA+pakend-pakdata-4)
	sta	LZ4_SRCEND_PTR+1
	;; Point LZ4_DST_PTR to the target area.
	lda	#<(dstpos)
	sta	LZ4_DST_PTR
	lda	#>(dstpos)
	sta	LZ4_DST_PTR+1
	;; Call the unpacker.
	jsr	lz4_unpack_data
	lda	#$37		; ROM on
	sta	$1
	cli
	jmp	$1fa9

copy_data_temparea:	.(
	lda	#<pakdata
	sta	p1
	lda	#>pakdata
	sta	p1+1
	lda	#<(TEMPDATAAREA)
	sta	p2
	lda	#>(TEMPDATAAREA)
	sta	p2+1
	ldy	#(pakend-pakdata)/256+1
	ldx	#0
l1:	lda	$ffff,x
p1=*-2
	sta	$ffff,x
p2=*-2
	sta	$428,x
	dex
	bne	l1
	inc	p1+1
	inc	p2+1
	dey
	bpl	l1
	rts
	.)

#include	"lz4.asm"

;;;place packed data here
pakdata:
 	.bin	0,0,"bokanoid-desire.bin.lz4"
pakend:
