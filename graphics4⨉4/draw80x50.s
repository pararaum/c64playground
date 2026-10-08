;;; Standalone 80x50 block-drawing routine for C64
;;; Original code by Aleksi Eeben, https://csdb.dk/release/?id=213334
;;; Extracted from BASIC extension for use as standalone library component

	.include "zeropage.inc"

	.export	plot80x50
	.export	_plot80x50_blocks

	.data
;;; Block character lookup table.
;;; Patterns for all 16 combinations of 4 bits (2x2 block quadrants).
_plot80x50_blocks:
	dc.b	$20,$7e,$7b,$61,$7c,$e2,$ff,$ec,$6c,$7f,$62,$fc,$e1,$fb,$fe,$a0

bitvalue	equ	tmp1
color		equ	tmp2
xcoord		equ	tmp3
ycoord		equ	tmp4
screen		equ	ptr1
screenh		equ	ptr1+1

	.code
;;; Draw a pixel at (x, y) with given color using block characters.
;;; Calling convention:
;;;   X = x-coordinate (0-79)
;;;   Y = y-coordinate (0-49)
;;;   A = color (0-15) or $ff to unplot
;;;
;;; This is a standalone routine without BASIC ROM hooks, parsing, or keyboard input.
	.proc	plot80x50
	cpx	#80
	bcs	.illegal
	cpy	#50
	bcs	.illegal

	sta	color
	stx	xcoord
	sty	ycoord

	; Process x coordinate.
	; Divide by two to the screen column and keep the lower bit for block side.
	ldx	#1
	lda	xcoord
	lsr
	sta	xcoord
	bcc	.x_right
	ldx	#4
.x_right
	stx	bitvalue

	; Process y coordinate.
	; Divide by two to the screen row and keep the lower bit for block half.
	lda	ycoord
	lsr
	sta	ycoord
	bcc	.y_lower
	asl	bitvalue
.y_lower
	ldx	#0
	stx	screenh

	; Calculate screen address.
	asl
	asl
	adc	ycoord
	ldx	#3
.shift_addr
	asl
	rol	screenh
	dex
	bne	.shift_addr
	sta	screen

	lda	screenh
	ora	#$04
	sta	screenh

	; Find the current block pattern at this screen location.
	ldy	xcoord
	ldx	#$10
.find_block
	lda	_plot80x50_blocks,x
	cmp	(screen),y
	beq	.found_block
	dex
	bne	.find_block
.found_block
	lda	color
	bmi	.unplot

	; Plot: combine existing pattern with the current bit.
	txa
	ora	bitvalue
.write_block
	tax
	lda	_plot80x50_blocks,x
	sta	(screen),y

	; Update color RAM.
	lda	screenh
	eor	#$dc
	sta	screenh
	lda	color
	bpl	.set_color
	rts
.set_color
	sta	(screen),y
	rts

.unplot
	; Unplot: clear the bit from the existing pattern.
	stx	ycoord
	eor	bitvalue
	and	ycoord
	bpl	.write_block

.illegal
	rts
	.endproc
