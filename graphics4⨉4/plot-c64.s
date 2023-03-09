;;; Code by Aleksi Eeben, https://csdb.dk/release/?id=213334. This uses the normal block drawing characters.
	processor 6502


SkipComma	equ 	$aefd
GetByte		equ 	$b79e

ErrorX		equ 	$a437

CHRGET		equ 	$0073

NEWSTT		equ 	$a7ae
GONE		equ 	$a7e4

bitvalue	equ 	$02
color		equ 	$03
xcoord		equ 	$fb
ycoord		equ 	$fc
screen		equ 	$fd
screenh		equ 	$fe


	org	$0340

Init
	lda	#<PlotLink	; link to GONE
	sta	$0308
	lda	#>PlotLink
	sta	$0309
	rts


IllegalQuantity
	ldx	#14		; illegal quantity error
	jmp	ErrorX

GetValue
	jsr	SkipComma
GetValueNoComma
	jsr	GetByte
	txa
	rts

PlotLink
	ldy	#4
.chlp
	lda	($7a),y
	cmp	PlotCommand-1,y
	beq	.plot
	jmp	GONE
.plot
	dey
	bne	.chlp

	ldx	#5
.fwd
	jsr	CHRGET
	dex
	bne	.fwd

	jsr	Plot
	jmp	NEWSTT

Plot
	jsr	GetValueNoComma	; get x-coordinate (0-79)
	cmp	#80
	bcs	IllegalQuantity
	ldx	#1		; left side of block
	lsr
	sta	xcoord		; screen x
	bcc 	.2
	ldx	#4		; right side of block
.2
	stx	bitvalue

	jsr	GetValue	; get y-coordinate (0-49)
	cmp	#50
	bcs	IllegalQuantity
	lsr
	sta	ycoord		; screen y
	bcc	.3
	asl	bitvalue	; lower fourths of block if bit 0 in y was 1
.3
	ldx 	#0
	stx	screenh

	asl			; calculate address of screen line
	asl
	adc	ycoord		; * 5

	ldx	#3
.1
	asl
	rol	screenh
	dex
	bne	.1
	sta	screen		; * 8

	lda	screenh		; screen at $0400
	ora	#$04
	sta	screenh

	jsr	GetValue	; get color (0-16)
	cmp	#17
	bcs	IllegalQuantity
	dex			; convert to color RAM value or $ff for unplot
	stx	color

	ldy	xcoord
	ldx	#$10
.4
	lda	Blocks,x	; find bit pattern at screen location
	cmp	(screen),y
	beq	.5
	dex
	bne	.4		; or zero if no block graphics here (or blank)
.5
	lda	color
	bmi	.unplot

	txa
	ora	bitvalue	; plot
.ok
	tax
	lda	Blocks,x
	sta	(screen),y

	lda 	screenh		; set block color
	eor	#$dc
	sta	screenh
	lda	color		; unplot shouldn't touch color memory
	bmi	.nocolor
	sta	(screen),y
.nocolor
	rts

.unplot
	stx	ycoord		; unplot from which pattern (reusing ycoord zeropage location)
	eor	bitvalue	; a = $ff
	and	ycoord
	bpl	.ok		; always branch

Blocks
	dc.b	$20,$7e,$7b,$61,$7c,$e2,$ff,$ec,$6c,$7f,$62,$fc,$e1,$fb,$fe,$a0

PlotCommand
	dc.b	"PLOT"
