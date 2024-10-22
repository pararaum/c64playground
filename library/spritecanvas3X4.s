	.include	"LAMAlib-macros16.inc"
	.include	"LAMAlib-routines.inc"
	.include	"t7d/stackmacros.i"

	.importzp	ptr1

	.export	spritecanvas3X4_init
	.export	spritecanvas3X4_irq

MAXDELAY=31			; Must be 2^n-1!

	.bss
first_sprptr:			; First sprite buffer pointer (top-left sprite).
	.res	1
xpositions:			; Sprite x-positions.
	.res	3
xposmsb:			; Sprite x-position MSBs.
	.res	1
yposition:
	.res	1
colour:	.res	1		; Sprite colour.

	.code
.proc set_y_position
	tay
	dey
l1:
	cpy	$d012
	bcs	l1
	bcc	*		; Carry must be cleared by now.
	DELAY=*-1		; To skip how many NOPs...
	.repeat	::MAXDELAY
	NOP
	.endrepeat
	;; 0-th sprite.
	sta	$d001+2*0
	stx	$FFFF
	SPRPTR0=*-2
	inx
	;; 1-st sprite.
	sta	$d001+2*1
	stx	$FFFF
	SPRPTR1=*-2
	inx
	;; 2-nd sprite.
	sta	$d001+2*2
	stx	$FFFF
	SPRPTR2=*-2
	inx
	clc
	adc	#42
	rts
.endproc


	.code
.proc	spritecanvas3X4_init
	GetReturnAddrIntoPointer	ptr1
	lda	#0
	sta	xposmsb		; Clear all sprite MSBs.
	ldy	#4
	lda	(ptr1),y	; Get sprite buffer number.
	sta	first_sprptr
	ldy	#3
	lda	(ptr1),y	; Get y-position.
	sta	yposition
	ldy	#2
	lda	(ptr1),y	; x-position HI
	tax
	dey
	lda	(ptr1),y	; x-position LO
	sta	xpositions
	cpx	#0		; Starting with x-positions above 256?
	beq	nohix
	ldx	#1
	stx	xposmsb
nohix:
	addax	#48
	sta	xpositions+1
	cpx	#0
	beq	nohix2
	pha
	lda	#2
	ora	xposmsb
	sta	xposmsb
	pla
nohix2:
	addax	#48
	sta	xpositions+2
	cpx	#0
	beq	nohix3
	pha
	lda	#4
	ora	xposmsb
	sta	xposmsb
	pla
nohix3:
	ldy	#6		; Sprite pointer address HI.
	lda	(ptr1),y
	tax
	dey
	lda	(ptr1),y	; Sprite pointer address LO.
	stax	set_y_position::SPRPTR0
	incax
	stax	set_y_position::SPRPTR1
	incax
	stax	set_y_position::SPRPTR2
	ldy	#7		; Setup sprite colour...
	lda	(ptr1),y
	sta	colour
	ldy	#8		; Get delay time.
	lda	(ptr1),y	; Get delay in to A
	lsr			; Divide by two, as every NOP takes two cycles.
	and	#MAXDELAY	; Restrict to 0-31.
	eor	#$FF		; Inverse suptraction [https://www.nesdev.org/wiki/Synthetic_instructions].
	sec
	adc	#MAXDELAY
	sta	set_y_position::DELAY
	PointerAdjustedToStack		ptr1, 8
	rts
.endproc


.proc	spritecanvas3X4_irq
	lda	$d015		; Enable sprite
	ora	#$07
	sta	$d015
	lda	$d017		; Enable double width
	ora	#$07
	sta	$d017
	lda	$d01d		; Enable double height
	ora	#$07
	sta	$d01d
	ldx	colour
	.repeat	3,I
	lda	xpositions+I
	sta	$d000+2*I
	stx	$d027+I
	.endrepeat
	lda	xposmsb
	sta	$d010
	lda	yposition
	ldx	first_sprptr
	inc	$D020
	jsr	set_y_position
	jsr	set_y_position
	jsr	set_y_position
	jsr	set_y_position
	rts
.endproc

