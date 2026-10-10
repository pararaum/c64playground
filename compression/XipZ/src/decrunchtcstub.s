	.include	"t7d/basic.i"
	.export	tocopy
	.export	tocopy_end
	.export	tocopy_len
	.export	jump_to
	.export STUBCODEPOS
	.export	stubcode
	.import	__LOADADDR__

;;; The decruncher for the "tc" format, see timecrunch.hh. It is the
;;; routine of the Time Cruncher V5 (the disassembly is on
;;; codebase64.net), the bit reader and the grammar are the same. The
;;; data is decrunched backwards: it is read downwards from the end of
;;; the packed data and written downwards from the end of the output.
;;; The packed data has been moved to the bottom of the output area
;;; where the output overtakes it from above.

STUBCODEPOS = $400

;;; The zero page addresses are the ones of the original.
REPEAT = $F9			; Literal runs that are not followed by a match.
BITBUF = $FA			; Bits that have not been read yet, left aligned.
BITCNT = $FB			; Number of these bits.
SRC = $FC			; Read pointer, 2 bytes. The next byte to read.
DST = $FE			; Write pointer, 2 bytes. The last byte that is not written.
LEN = $8B			; Length of a run or a match.
COPY = $8C			; Source pointer of a copy, 2 bytes. The high byte is
				; also the high byte of the numbers that are read.
TMP = $8E
;;; Pointers for moving the data.
MVSRC = $F5
MVDST = $F7

	DEFAULT_DESTINATION=$800

	.segment	"EXEHDR"
START:	.word	thebrk
minuslen:
	.word	tocopy
	.byte	$9e,"2061"
thebrk:	brk
	brk
	brk

	.data
tocopy:
	;; 	.byte	"copy"
tocopy_end:
tocopy_len=tocopy_end-tocopy

	.code
	sei
	ldx	#0
stubcopyloop:			; The code is longer than 256 bytes.
	lda	stubcode,x
	sta	STUBCODEPOS,x
	lda	stubcode+$100,x
	sta	STUBCODEPOS+$100,x
	dex
	bne	stubcopyloop
	jmp	STUBCODEPOS

stubcode:
	;; Danger this code originates at the indented stub code position!
	.org	STUBCODEPOS
realstubcode:
	lda	#$34
	sta	1		; Only memory.
	;; Move the packed data to its place. It is moved upwards if the
	;; place is above the source, so the direction depends on it.
	lda	#<tocopy_end
	sta	MVSRC
	lda	#>tocopy_end
	sta	MVSRC+1
	lda	#0
	MVDSTLO = *-1
	sta	MVDST
	lda	#0
	MVDSTHI = *-1
	sta	MVDST+1
	lda	MVDST
	cmp	MVSRC
	lda	MVDST+1
	sbc	MVSRC+1
	bcs	movedown
	ldy	#0		; Move downwards, copy ascending.
	ldx	mvpages
	beq	asc_rest
asc_page:
	lda	(MVSRC),y
	sta	(MVDST),y
	iny
	bne	asc_page
	inc	MVSRC+1
	inc	MVDST+1
	dex
	bne	asc_page
asc_rest:
	cpy	mvrest
	beq	moved
	lda	(MVSRC),y
	sta	(MVDST),y
	iny
	bne	asc_rest	; Always, mvrest is less than 256.
movedown:			; Move upwards, copy descending.
	clc
	lda	MVSRC+1
	adc	mvpages
	sta	MVSRC+1
	clc
	lda	MVDST+1
	adc	mvpages
	sta	MVDST+1
	ldy	mvrest
	beq	desc_pages
desc_rest:
	dey
	lda	(MVSRC),y
	sta	(MVDST),y
	cpy	#0
	bne	desc_rest
desc_pages:
	ldx	mvpages
	beq	moved
desc_page:
	dec	MVSRC+1
	dec	MVDST+1
	ldy	#0
desc_byte:
	dey
	lda	(MVSRC),y
	sta	(MVDST),y
	cpy	#0
	bne	desc_byte
	dex
	bne	desc_page
moved:
	lda	#0
	sta	REPEAT
	sta	BITCNT
	lda	#0
	SRCLO = *-1
	sta	SRC
	lda	#0
	SRCHI = *-1
	sta	SRC+1
	lda	#0
	DSTLO = *-1
	sta	DST
	lda	#0
	DSTHI = *-1
	sta	DST+1
	;; The decruncher. After readbits the register X is $ff, the carry
	;; is clear and Y is the number that was read.
main:	ldx	#2		; A token has three bits.
	jsr	readbits
	beq	next		; No literals.
	cmp	#6
	bcc	literal
	and	#1		; A longer run: four or eight bits.
	tay
	jsr	readtab
	adc	#6
	bcc	literal
	tax			; The escape, v > 249: a number of runs.
	jsr	readbits
	sta	REPEAT
	bpl	main		; Always.
literal:
	sta	LEN
	lda	SRC		; The literals are below the last byte that was read.
	sec
	sbc	LEN
	sta	SRC
	sta	COPY
	lda	SRC+1
	sbc	#0
	sta	SRC+1
	sta	COPY+1
	jsr	copy
next:	ldx	REPEAT		; Is there a match?
	beq	match
	dec	REPEAT
	bpl	main		; Always.
match:	jsr	readbits	; X is zero, one bit.
	beq	len_long
	jsr	readtab		; Length two, eight bits of offset (Y is 1).
	ldx	#2
	stx	LEN
	bcc	matchcopy	; Always.
len_long:
	inx
	jsr	readbits	; One bit.
	beq	len3
	inx
	jsr	readbits	; One bit, selects four or eight bits.
	jsr	readtab
	adc	#1
len3:	adc	#3		; Length is the number plus four, or three.
	sta	LEN
	inx
	jsr	readbits	; One bit, selects eight or 8+STEP bits.
	iny
	jsr	readtab
matchcopy:
	adc	DST		; The source is the offset above the destination.
	sta	COPY
	lda	COPY+1
	adc	DST+1
	sta	COPY+1
	jsr	copy
	beq	main		; Always.
	;; Copy LEN bytes from (COPY)+1.. to (DST)+1.. after moving DST down.
copy:	ldy	LEN
	sec
	lda	DST
	sbc	LEN
	sta	DST
	bcs	copy_1
	dec	DST+1
copy_1:	lda	(COPY),y
	sta	(DST),y
	dey
	bne	copy_1
	rts
	;; Read X+1 bits into A (and the high bits into COPY+1), Y=0 if A=0.
readtab:
	ldx	tab,y
readbits:
	lda	#0
	sta	COPY+1
rb_loop:
	ldy	BITCNT
	beq	refill
rb_bit:	asl	BITBUF
	rol
	rol	COPY+1
	dec	BITCNT
	dex
	bpl	rb_loop
	tay
	rts
refill:	sta	TMP		; Get the next byte, Y is zero.
	lda	(SRC),y
	sta	BITBUF
	lda	#8
	sta	BITCNT
	lda	TMP
	ldy	SRC
	bne	rf_nb
	dec	SRC+1
rf_nb:	dec	SRC
	cpy	#0		; The byte below the data has been fetched, finished.
ENDLO = *-1
	bne	rb_bit
	ldy	SRC+1
	cpy	#0
ENDHI = *-1
	bne	rb_bit
	lda	#$37
	sta	1
	jmp	*
	jump_to=*-2
	;; Widths of the numbers: 4 bits, 8 bits and 8+STEP bits, minus one.
tab:	.byte	3, 7, 0
	STEP7 = *-1
mvpages:
	.byte	0
mvrest:	.byte	0

stubcodelen = *-realstubcode

	.export	jump_to_offset=jump_to-realstubcode+stubcode-START
	.export mvdstlo_offset=MVDSTLO-realstubcode+stubcode-START
	.export mvdsthi_offset=MVDSTHI-realstubcode+stubcode-START
	.export srclo_offset=SRCLO-realstubcode+stubcode-START
	.export srchi_offset=SRCHI-realstubcode+stubcode-START
	.export dstlo_offset=DSTLO-realstubcode+stubcode-START
	.export dsthi_offset=DSTHI-realstubcode+stubcode-START
	.export endlo_offset=ENDLO-realstubcode+stubcode-START
	.export endhi_offset=ENDHI-realstubcode+stubcode-START
	.export step7_offset=STEP7-realstubcode+stubcode-START
	.export mvpages_offset=mvpages-realstubcode+stubcode-START
	.export mvrest_offset=mvrest-realstubcode+stubcode-START
	.export stubcodelen_value=stubcodelen
