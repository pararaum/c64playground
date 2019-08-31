;;; LZ4 data decompressor for C64, based on the code of Peter Ferrie
;	[http: //pferrie.host22.com/misc/appleii.htm], parts were also
;	taken from the xBIOS page
;	[https://xxl.atari.pl/lz4-decompressor/.

#ifdef LZ4_DEFAULT_POINTERS
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
#endif
#ifldef LZ4_WORKING_BEE
#else	
;;; Adress to store something while uncrunching. If this is zero, no output.
LZ4_WORKING_BEE = $0
#endif

;;; Unpacker entrypoint, this routine will unpack the data, remember that the LZ4_* variable have to be set correctly.
lz4_unpack_data:	.(
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
	rts

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

putdst:	.(
	sta	(LZ4_DST_PTR),y
	inc	LZ4_DST_PTR
	bne	l1
	inc	LZ4_DST_PTR+1
l1:	rts
	.)

;;; Get the next byte from the source.
;;; In
;;; Y=0
;;; Out A=Byte
getsrc:	.(
	lda	(LZ4_SRC_PTR),y
	inc	LZ4_SRC_PTR
	bne	l1
	inc	LZ4_SRC_PTR+1
l1:
#if LZ4_WORKING_BEE
	sta	LZ4_WORKING_BEE
#endif
	rts
	.)

	.)			; End block for lz4_unpack_data
