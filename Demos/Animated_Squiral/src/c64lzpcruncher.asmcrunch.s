	.include	"t7d/libt7d.i"
	.include	"t7d/memoryfunctions.i"
	.include	"t7d/vic/vicmacros.i"
	.include	"t7d/kernal.i"

	.import	popax

	.export	_init_asm
	.export	_crunchLZP
	.export         __EXEHDR__: absolute = 1

	MODELBUF = $400

	.bss
infile:		.res	1
outfile:	.res	1
cbyte:		.res	1	; Current byte
hash:	.res	1		; Hash value
maskidx:	.res	1	; Current bit in mask, initialise to $FF as it will be incremented in emit...
byteswritten:	.res	1	; Number of actual literal bytes written
buf9niner:	.res	9	; Nine byte output buffer for handling mask and literals/runs.
	mask=buf9niner		; Mask
	buf9=buf9niner+1	; Real buffer

	.segment	"EXEHDR"
	cld
Start:

	.code
.proc	irqroutine
	asl	$d019
	jsr	$1006
	jmp	$EA31
.endproc

.proc	_init_asm
	CopyFileIntoMemoryDown $1000, "assets/Blitzermarathon.sid", $7c+2
	lda	#0
	jsr	$1000
	sei
	jsr	_disable_cia_irq
	SetIRQ314Pointer	irqroutine
	EnableIRQatRasterline	0
	cli
	rts
.endproc

.proc	model_init
	lda	#0
	sta	hash		; Begin with hash=0.
	tax
	;; Clear model.
l1:	sta	MODELBUF,x
	dex
	bne	l1
	rts
.endproc

.proc	readbyte
	ldx	infile		; Read byte from input.
	jsr	CHKIN
	jsr	CHRIN
	sta	cbyte
	rts
.endproc

.proc	writebyte
	pha
	ldx	outfile
	jsr	CHKOUT
	pla
	jmp	CHROUT
.endproc


.proc	writegroup
	lda	#0
	sta	curridx		; Clear current index.
loop:	ldx	#0
	curridx=*-1
	cpx	byteswritten
	maxX=*-1
	beq	out
	lda	buf9niner,x
	jsr	writebyte
	inc	curridx
	jmp	loop
out:
	rts
.endproc
.proc	flushgroup
	jmp	writegroup
.endproc
	
;;; Input: hash
;;; Modifies: A, X
.proc	model_predict
	ldx	hash
	lda	MODELBUF,x
	rts
.endproc


.proc	model_advance
	asl	hash
	asl	hash
	asl	hash
	clc
	adc	hash
	sta	hash
	rts
.endproc

;;; Modifies: A,X,hash
.proc	model_update
	ldx	hash
	sta	MODELBUF,x
	jmp	model_advance
.endproc


;;; Input: C
.proc	emit_byte
	php
	rol	mask		; And put it into the mask.
	inc	maskidx		; Which bit was handled?
	ldx	maskidx
	plp
	bcs	nobyteout
	inc	byteswritten
	sta	buf9,x
nobyteout:
	cpx	#7		; Was MSB?
	bne	notfull		; Not a full group.
	jsr	writegroup
	ldx	#0		; Reset to first byte.
	stx	mask		; Needed for last byte so that we have a clean mask at the end when writing a partial group.
	stx	byteswritten	; And reset the number of actual bytes written.
	dex
	stx	maskidx		; Start with $FF.
notfull:
	rts
.endproc


.proc	emit_prediction
	sec
	jmp	emit_byte
.endproc


.proc	emit_literal
	clc
	jmp	emit_byte
.endproc

;;; extern void crunchLZP(unsigned short size, int infile, int outfile);
.proc	_crunchLZP
	sta	outfile
	jsr	popax
	sta	infile
	jsr	popax		; size not used
	jsr	model_init
	lda	#$FF
	sta	maskidx
	lda	#0
	sta	byteswritten
crunchloop:
	jsr	readbyte
	bcs	out
	jsr	READST
	bne	out
	jsr	model_predict	; What is the next prediction?
	cmp	cbyte		; Equal?
	bne	no_prediction
	jsr	emit_prediction
	jmp	cont_upd
no_prediction:
	jsr	emit_literal
	lda	cbyte
cont_upd:
	jsr	model_update
	jmp	crunchloop

out:
	lda	#0		; Zero run length is EOF.
	;; TODO: EOF marker!
	jmp	flushgroup
.endproc
