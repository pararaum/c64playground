;;; xa -o copy_n_uncompress.stub -M -I ../include/ -l /dev/stdout 'copy_n_uncompress.asm'
;;; Create with "lz4 -9 -BD --no-content-size --no-frame-crc".
;;; Further reading:
;;;  • https://xxl.atari.pl/lz4-decompressor/
;;;  • https://create.stephan-brumme.com/smallz4/
;;; Warning! This is a very primitive decoder and if the encoder decides that compression does harm the frame size will have the MSB set and the data is to be copied literally. This variant is *ignored* here, so if you create such a file and try to decrunch it then it will explode in your face! Beware!

	VARTAB = $2D
	VARTABHI = VARTAB+1
DECRUNCHTGT		=	$0360	; So there is something happening on the screen while decrunching.
DEFAULTDECRUNCHAT	=	$0801-2 ;Load address is in front.
;;; End of source data (byte after the end)
LZ4_SRCEND_PTR		=	$57
;;; Storage for repeat or copy count
LZ4_REPEAT_COUNTER16	=	$5b
;;; Storage for the delta offset for match copying
LZ4_DELTA_COPYOP16	=	$5d

	.word	$0801
	*=$0801
	.word	end_of_basic
	.word	7
	.byte	$9e,$20,"2070 : ",$8f, " T7D"
end_of_basic:
	.byte	0,0,0

	sei
	lda	#$34
	sta	$1
	lda	VARTAB
	sta	src
	lda	VARTABHI
	sta	src+1
lzql1:
	;; https://wiki.nesdev.com/w/index.php/Synthetic_instructions for decremt 16 bit. Do not forget!
	lda	src
	bne	nodec1
	dec	src+1
nodec1:	dec	src
	lda	dst
	bne	nodec2
	dec	dst+1
nodec2:	dec	dst
	lda	$FFFF
	src=*-2
	sta	$ff80
	dst=*-2
	lda	src
	cmp	#<(ENDdecrunch-DECRUNCHTGT+decrunchcode)
	bne	lzql1
	lda	src+1
	cmp	#>(ENDdecrunch-DECRUNCHTGT+decrunchcode)
	bne	lzql1
	;; Now copy the decrunch code the save area. X has to be zero.
	ldx	#0
	jsr	lzql2
	;; Destination is pointing to the beginning of the data. We have to skip the normal LZ4 header (of 11 bytes). See also https://github.com/pararaum/lz4-68k/blob/master/lz4_frame.asm.
	lda	dst
	clc
	adc	#4+7
	sta	LZ4_SRC_PTR
	lda	dst+1
	adc	#0		; Add the carry
	sta	LZ4_SRC_PTR+1
	;; $ff80 is the end address as we started the copy there. The EndMark are four zeros, see https://github.com/lz4/lz4/blob/dev/doc/lz4_Frame_format.md.
	lda	#<($ff80-4-1)
	sta	LZ4_SRCEND_PTR
	lda	#>($ff80-4-1)
	sta	LZ4_SRCEND_PTR+1
	jmp	DECRUNCHTGT
lzql2:	lda	decrunchcode,x
	sta	DECRUNCHTGT,x
	dex
	bne	lzql2
	rts
decrunchcode:
	* = DECRUNCHTGT
;;; LZ4 data decompressor for C64, based on the code of Peter Ferrie
;	[http://pferrie.host22.com/misc/appleii.htm], parts were also
;	taken from the xBIOS page
;	[https://xxl.atari.pl/lz4-decompressor/.

;;; Unpacker entrypoint, this routine will unpack the data, remember that the LZ4_* variable have to be set correctly.
lz4_unpack_data:
	ldy	#0		; Clear for indexed access.

parsetoken
	jsr	getsrc		; Next Byte into accumulator
	pha			; Put on stack
	lsr			; A >>= 4, get upper nibble
	lsr
	lsr
	lsr
	beq	copymatches	; Upper nibble contain literals, if zero then none.
	jsr	buildcount
	tax
	jsr	docopy
	lda	LZ4_SRCEND_PTR
	cmp	LZ4_SRC_PTR
	lda	LZ4_SRCEND_PTR+1
	sbc	LZ4_SRC_PTR+1
	bcc	done

copymatches:	.(
	;; Following the the literals (if any) is the match copy operation [https://github.com/lz4/lz4/blob/master/doc/lz4_Block_format.md]. The next two bytes are the offset in little endian, very convenient for a 6510.
	jsr	getsrc
	sta	LZ4_DELTA_COPYOP16
	jsr	getsrc
	sta	LZ4_DELTA_COPYOP16+1
	pla			; Now get the matchlength
	and	#$0f
	jsr	buildcount	; Build the count in X and zp.
	clc
	adc	#4
	tax
	beq	l1
	bcc	l1
	inc	LZ4_REPEAT_COUNTER16+1
l1	lda	LZ4_SRC_PTR+1		; Store source address on stack.
	pha
	lda	LZ4_SRC_PTR
	pha
	sec			; Subtract LZ4_DELTA_COPYOP16 from destination (as this is the offset)
	lda	LZ4_DST_PTR
	sbc	LZ4_DELTA_COPYOP16
	sta	LZ4_SRC_PTR
	lda	LZ4_DST_PTR+1
	sbc	LZ4_DELTA_COPYOP16+1
	sta	LZ4_SRC_PTR+1
	jsr	docopy		; Copy the data from the newly set source.
	pla			; Restore old source pointer.
	sta	LZ4_SRC_PTR
	pla
	sta	LZ4_SRC_PTR+1
	jmp	parsetoken
	.)
done:	pla
	lda	#$37
	sta	1
	;; Basic RUN. ［https://codebase64.org/doku.php?id=base:runbasicprg］
	lda	LZ4_DST_PTR
	sta	VARTAB		; end of BASIC program text+1 / start of numeric variables
	sta	$ae		; End address of LOAD, …
	lda	LZ4_DST_PTR+1
	sta	VARTABHI
	sta	$af
	lda	#0
	sta	$0800		; Make sure this is a zero byte otherwise you will always get a SYNTAX ERROR.
	jsr	$a659		; CLR ［https://www.pagetable.com/c64disasm/#A659］.
	jsr	$a533		; Rechain lines ［https://www.pagetable.com/c64disasm/#A533］.
	cli
	jmp	$a7ae		; Warm start

docopy:
	jsr	getput
	dex
	bne	docopy
	dec	LZ4_REPEAT_COUNTER16+1
	bne	docopy
	rts

buildcount:	.(
	ldx	#1
	stx	LZ4_REPEAT_COUNTER16+1
	cmp	#$0f
	bne	l2
m1	sta	LZ4_REPEAT_COUNTER16
	jsr	getsrc
	tax
	clc
	adc	LZ4_REPEAT_COUNTER16
	bcc	l1
	inc	LZ4_REPEAT_COUNTER16+1
l1	inx
	beq	m1
l2:	rts
	.)

getput
	jsr	getsrc

putdst:				; Put byte at destination pointer.
	sta	DEFAULTDECRUNCHAT
	;;; Destination pointer
	LZ4_DST_PTR=*-2
	inc	LZ4_DST_PTR
	bne	putdst_l1
	inc	LZ4_DST_PTR+1
putdst_l1:
	rts

;;; Get the next byte from the source.
;;; In
;;; Y=0
;;; Out A=Byte
getsrc:
	lda	$FFFF
	;;; Source (compressed data) pointer
	LZ4_SRC_PTR=*-2
	inc	LZ4_SRC_PTR
	bne	getsrc_l1
	inc	LZ4_SRC_PTR+1
getsrc_l1:
	rts
ENDdecrunch:
	;; Append your LZ4-compressed data here.
