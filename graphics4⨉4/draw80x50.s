;;; Standalone 80x50 block-drawing routine for C64
;;; Original code by Aleksi Eeben, https://csdb.dk/release/?id=213334
;;; Extracted from BASIC extension for use as standalone library component
	processor 6502

bitvalue	equ	$02
color		equ	$03
xcoord		equ	$fb
ycoord		equ	$fc
screen		equ	$fd
screenh		equ	$fe

;;; Draw a pixel at (x, y) with given color using block characters
;;; Input:
;;;   A = x-coordinate (0-79)
;;;   X = y-coordinate (0-49)
;;;   bitvalue = color (0-15) or $ff to unplot
;;;
;;; Uses zero-page locations: $02, $03, $fb, $fc, $fd, $fe
Draw
	cmp	#80
	bcs	.illegal
	cpx	#50
	bcs	.illegal

	; Process x coordinate
	; Divide by 2 to get screen column, save lower bit for block side
	ldx	#1		; left side of block
	lsr
	sta	xcoord		; screen x
	bcc	.x_right
	ldx	#4		; right side of block
.x_right
	stx	bitvalue

	; Process y coordinate (in X register from input)
	; Divide by 2 to get screen row, save lower bit for block half
	txa
	lsr
	sta	ycoord		; screen y
	bcc	.y_lower
	asl	bitvalue	; lower half of block if bit 0 in y was 1
.y_lower
	ldx	#0
	stx	screenh

	; Calculate screen address = y * 40 * 8
	asl			; multiply by 2
	asl			; multiply by 4
	adc	ycoord		; multiply by 5
	ldx	#3
.shift_addr
	asl
	rol	screenh
	dex
	bne	.shift_addr
	sta	screen		; multiply by 8

	; Screen base at $0400
	lda	screenh
	ora	#$04
	sta	screenh

	; Get current block character at this location
	ldy	xcoord
	ldx	#$10
.find_block
	lda	Blocks,x
	cmp	(screen),y
	beq	.found_block
	dex
	bne	.find_block	; or zero if no block graphics here
.found_block
	lda	color
	bmi	.unplot

	; Plot: combine existing pattern with new bit
	txa
	ora	bitvalue
.write_block
	tax
	lda	Blocks,x
	sta	(screen),y

	; Set block color in color RAM
	lda	screenh
	eor	#$dc		; color RAM base is $d800
	sta	screenh
	lda	color
	bmi	.no_color	; unplot shouldn't touch color memory
	sta	(screen),y
.no_color
	rts

.unplot
	; Unplot: clear the bit from existing pattern
	stx	ycoord		; save current pattern index
	eor	bitvalue	; a = $ff
	and	ycoord
	bpl	.write_block	; always branch

.illegal
	rts

;;; Block character lookup table
;;; Patterns for all 16 combinations of 4 bits (2x2 block quadrants)
Blocks
	dc.b	$20,$7e,$7b,$61,$7c,$e2,$ff,$ec,$6c,$7f,$62,$fc,$e1,$fb,$fe,$a0
