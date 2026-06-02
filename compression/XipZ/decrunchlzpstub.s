	.include	"t7d/basic.i"
	.export	tocopy
	.export	tocopy_end
	.export	tocopy_len

STUBCODEPOS = $400
;;; Current hash value.
HASH = $A4
;;; Source data pointer. (2 Bytes)
SRCDATAPTR = $A5
;;; Current replay length.
LENGTH = $A7
;;; Next Mask bit number.
MASKIDX = $A8
;;; Current Mask.
MASK = $A9

	DEFAULT_DESTINATION=$800

	.segment	"EXEHDR"
	.word	thebrk
minuslen:
	.word	$10000-tocopy_len
	.byte	$9e,"2061"
thebrk:	brk
	brk
	brk

	.data
tocopy:
	.incbin	"lzp.hh.prg"
	;; 	.byte	"copy"
tocopy_end:
tocopy_len=tocopy_end-tocopy
	
	.code
	sei
	sta	HASH		; A=0 after SYS.
	ldx	#stubcodelen
stubcopyloop:
	lda	stubcode-1,x
	sta	STUBCODEPOS-1,x
	dex
	bne	stubcopyloop
	lda	minuslen
	sta	SRCDATAPTR
	lda	minuslen+1
	sta	SRCDATAPTR+1
	lda	#<DEFAULT_DESTINATION
	sta	DSTDATAPTR
	lda	#>DEFAULT_DESTINATION
	sta	DSTDATAPTR+1
	jmp	STUBCODEPOS

stubcode:
	;; Danger this code originates at the indented stub code position!
	.org	STUBCODEPOS
realstubcode:
	lda	#0
	tay
l1:	sta	model,y
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
	;; Now start the actual decrunch.
	;; Get Mask. Y=0 from loop above.
	sty	MASKIDX
decrunchloop:
	jsr	next_mask
	beq	literal
	jsr	next_byte	; Get run length.
	cmp	#0
	beq	finished
replay:
	;; A=length.
	sta	LENGTH
replayloop:
	jsr	predict
	jsr	output
	jsr	advance
	dec	LENGTH
	bne	replayloop
	beq	decrunchloop
literal:
	jsr	next_byte
	jsr	output
	jsr	update
	jmp	decrunchloop
	;; Get next byte.
next_byte:
	lda	(SRCDATAPTR),y
	iny
	bne	nb_out
	inc	SRCDATAPTR+1
nb_out:	rts
next_mask:
	asl	MASKIDX
	bne	nm_stillgoing
	lda	#1
	sta	MASKIDX
	jsr	next_byte
	sta	MASK
nm_stillgoing:
	lda	MASKIDX
	and	MASK
	rts
finished:
	lda	#$37
	sta	1
	jmp	*
update:
	ldx	HASH
	sta	model,x
advance:
	asl	HASH
	asl	HASH
	asl	HASH
	eor	HASH
	sta	HASH
	rts
predict:
	ldx	HASH
	lda	model,x
	rts
output:
	sta	*
DSTDATAPTR = *-2
	inc	DSTDATAPTR
	bne	op_out
	inc	DSTDATAPTR+1
op_out:	rts

model:	.res	0
stubcodelen = *-realstubcode

