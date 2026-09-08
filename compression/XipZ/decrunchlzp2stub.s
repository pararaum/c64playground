	.include	"t7d/basic.i"
	.export	tocopy
	.export	tocopy_end
	.export	tocopy_len
	.export	jump_to
	.export STUBCODEPOS
	.export	stubcode
	.import	__LOADADDR__

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
	sta	HASH		; A=0 after SYS.
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
next_mask:			; Get the next mask bit.
	asl	MASKIDX
	bne	nm_stillgoing
	lda	#1
	sta	MASKIDX
	jsr	next_byte
	sta	MASK
nm_stillgoing:
	lda	MASKIDX
	and	MASK
	beq	literal
predict:			; Predict the next value.
	ldx	HASH
	lda	model,x
	jsr	output
	jsr	advance
	jmp	decrunchloop
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
	bne	nb_out		; If HI rolls over to $00 we have reached the end.
finished:
	lda	#$37
	sta	1
	jmp	*
	jump_to=*-2
nb_out:	rts
update:				; Modifies: A, X
	ldx	HASH
	sta	model,x
advance:			; Modifies: A
	asl	HASH
	asl	HASH
	asl	HASH
	asl	HASH
	asl	HASH
	eor	HASH
	sta	HASH
	rts
output:				; Modifies: -
	sta	64738
DSTDATAPTR = *-2
	inc	DSTDATAPTR
	bne	op_out
	inc	DSTDATAPTR+1
op_out:	rts

model:	.res	0
stubcodelen = *-realstubcode

	.export minuslenlo_offset=MINUSLENLO-START
	.export minuslenhi_offset=MINUSLENHI-START

	.export	jump_to_offset=jump_to-realstubcode+stubcode-START
	.export	dstdataptr_offset=DSTDATAPTR-realstubcode+stubcode-START
	.export upcopystc_offset=UPCPYSRC-realstubcode+stubcode-START
