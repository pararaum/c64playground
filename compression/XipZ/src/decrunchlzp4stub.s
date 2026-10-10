	.include	"t7d/basic.i"
	.export	tocopy
	.export	tocopy_end
	.export	tocopy_len
	.export	jump_to
	.export STUBCODEPOS
	.export	stubcode
	.import	__LOADADDR__

STUBCODEPOS = $400
;;; Low bytes of the position table, indexed by the hash.
TABLO = $600
;;; High bytes of the position table.
TABHI = $700
;;; Hash of the last bytes.
HASH = $A4
;;; Source data pointer. (2 Bytes)
SRCDATAPTR = $A5
;;; Mask bits, a set sentinel bit marks the end.
MASK = $A7
;;; Remaining length of the current run.
LEN = $A8
;;; The byte currently being output.
BYTE = $A9
;;; Shift of the hash and minimum length of a run, see lzp.cc.
HASHSHIFT = 4
MINLEN = 2

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
	ldx	#stubcodelen
stubcopyloop:
	lda	stubcode-1,x
	sta	STUBCODEPOS-1,x
	dex
	bne	stubcopyloop
	lda	#0
	MINUSLENLO = *-1
	sta	SRCDATAPTR
	lda	#0
	MINUSLENHI = *-1
	sta	SRCDATAPTR+1
	jmp	STUBCODEPOS

stubcode:
	;; Danger this code originates at the indented stub code position!
	.org	STUBCODEPOS
realstubcode:
	ldy	#0
	sty	HASH
	sty	MASK		; Forces the first mask byte to be read.
l1:	lda	DSTDATAPTR	; Every entry points to the start of the output.
	sta	TABLO,y
	lda	DSTDATAPTR+1
	sta	TABHI,y
	dey
	bne	l1
	lda	#$34
	sta	1		; Only memory.
l3:	dec	UPCPYSRC+1
	dec	UPCPYDST+1
l2:	lda	tocopy_end,y
UPCPYSRC = *-2
	sta	a:$0,y
UPCPYDST = *-2
	dey
	bne	l2
	lda	UPCPYSRC+1
	cmp	#7
	bne	l3
	;; Now start the actual decrunch. Y=0 from loop above.
decrunchloop:
	jsr	getbit
	bcc	literal
	jsr	next_byte	; Run: length minus MINLEN.
	clc
	adc	#MINLEN
	sta	LEN
	ldx	HASH		; Where did this context occur last?
	lda	TABLO,x
	sta	COPYSRC
	lda	TABHI,x
	sta	COPYSRC+1
copyloop:
	lda	a:$0000
COPYSRC = *-2
	jsr	put
	inc	COPYSRC
	bne	cp_nocarry
	inc	COPYSRC+1
cp_nocarry:
	dec	LEN
	bne	copyloop
	beq	decrunchloop	; Always.
literal:
	jsr	next_byte
	jsr	put
	jmp	decrunchloop
	;; Output the byte in A and update the table and the hash.
put:	sta	BYTE
	ldx	HASH
	lda	DSTDATAPTR	; Position of this byte.
	sta	TABLO,x
	lda	DSTDATAPTR+1
	sta	TABHI,x
	lda	HASH
	.repeat	HASHSHIFT
	asl
	.endrepeat
	eor	BYTE
	sta	HASH
	lda	BYTE
	;; Fall through to output.
output:				; Modifies: X
	sta	64738
DSTDATAPTR = *-2
	inc	DSTDATAPTR
	bne	op_chk
	inc	DSTDATAPTR+1
op_chk:	ldx	DSTDATAPTR	; Reached the end address? Only X is free here.
	cpx	#0
ENDLO = *-1
	bne	op_out
	ldx	DSTDATAPTR+1
	cpx	#0
ENDHI = *-1
	beq	finished
op_out:	rts
	;; Get the next mask bit into the carry.
getbit:
	asl	MASK
	bne	gb_out
	jsr	next_byte	; Only the sentinel was left, get new mask.
	sec
	rol
	sta	MASK
gb_out:	rts
	;; Get next byte.
next_byte:
	lda	(SRCDATAPTR),y
	iny
	bne	nb_out
	inc	SRCDATAPTR+1
nb_out:	rts
	;; Entered from output when the end address has been reached.
finished:
	lda	#$37
	sta	1
	jmp	*
	jump_to=*-2

stubcodelen = *-realstubcode

	.export minuslenlo_offset=MINUSLENLO-START
	.export minuslenhi_offset=MINUSLENHI-START

	.export	jump_to_offset=jump_to-realstubcode+stubcode-START
	.export	dstdataptr_offset=DSTDATAPTR-realstubcode+stubcode-START
	.export endlo_offset=ENDLO-realstubcode+stubcode-START
	.export endhi_offset=ENDHI-realstubcode+stubcode-START
	.export upcopystc_offset=UPCPYSRC-realstubcode+stubcode-START
