	.include	"t7d/basic.i"
	.export	tocopy
	.export	tocopy_end
	.export	tocopy_len
	.export	jump_to
	.export STUBCODEPOS
	.export	stubcode
	.import	__LOADADDR__

STUBCODEPOS = $400
;;; Order-1 table, indexed by the last byte.
MODEL = $500
;;; Source data pointer. (2 Bytes)
SRCDATAPTR = $A5
;;; The last byte.
BYTE1 = $A7
;;; Mask bits, a set sentinel bit marks the end.
MASK = $A9
;;; Current prediction.
PRED = $AA

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
	lda	#0
	tay
l1:	sta	MODEL,y
	dey
	bne	l1
	sta	BYTE1
	sta	MASK		; Forces the first mask byte to be read.
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
	ldx	BYTE1
	lda	MODEL,x
	sta	PRED
	jsr	getbit
	bcc	literal
	lda	PRED		; Hit, carry is still set.
	bcs	emit		; Always.
literal:
	jsr	next_byte
emit:	jsr	output
	ldx	BYTE1		; Update the model.
	sta	MODEL,x
	sta	BYTE1
	jmp	decrunchloop
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

stubcodelen = *-realstubcode

	.export minuslenlo_offset=MINUSLENLO-START
	.export minuslenhi_offset=MINUSLENHI-START

	.export	jump_to_offset=jump_to-realstubcode+stubcode-START
	.export	dstdataptr_offset=DSTDATAPTR-realstubcode+stubcode-START
	.export endlo_offset=ENDLO-realstubcode+stubcode-START
	.export endhi_offset=ENDHI-realstubcode+stubcode-START
	.export upcopystc_offset=UPCPYSRC-realstubcode+stubcode-START
