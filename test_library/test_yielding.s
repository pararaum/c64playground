	.include	"t7d/pseudo/yielding.i"
	.include	"t7d/libt7d.i"
	.include	"t7d/kernal.i"
	.include	"t7d/vic/vicmacros.i"
	.macpack	cbm

	.importzp	ptr1, ptr2

RASTERLINE=249
SCREENBASE=$400

	.data
t7d:	scrcode	"the 7th division"
t7d_end:

	.segment	"LOWCODE"
muzak:	.incbin	"Vibrations.sid",$7c+2
muzak_play=muzak+3

	.segment	"EMPTY"
	.segment	"INIT"
	.segment	"ONCE"
	.segment	"STARTUP"

	sei
	lda	#0
	jsr	muzak
	SetIRQ314Pointer	irq
	lda	#0
	sta	$d011
	jsr	busywait_frame_pm
	lda	#0
	sta	$d021
	jsr	_disable_cia_irq
	EnableIRQatRasterline	RASTERLINE
	SetupYielding	yfunction
	cli
	lda	#0
	jsr	_fill_colour_ram
	lda	#<SCREENBASE
	ldx	#>SCREENBASE
	ldy	#' '
	jsr	fill_1000_bytes
	jmp	*

	.code
irq:	asl	$d019
	jsr	muzak_play
	CallYielding
out:	jmp	EXITIRQ

	.data
fcols:	.byte	4, 2, $b, 0
fcols_end:

	.code
yfunction:
	ldy	#0
l2:	lda	fcols,y
	sta	$d020
	ldx	#7
l1:	Yield
	dex
	bne	l1
	iny
	cpy	#fcols_end-fcols
	bne	l2
	lda	#$1b
	sta	$d011
	ldx	#0
l3:	lda	#$5a
	sta	SCREENBASE,x
	sta	SCREENBASE+24*40,x
	lda	#3
	sta	$d800,x
	sta	$d800+24*40,x
	Yield
	inx
	cpx	#40
	bne	l3
	lda	#<SCREENBASE
	sta	ptr1
	lda	#>SCREENBASE
	sta	ptr1+1
	lda	#<$D800
	sta	ptr2
	lda	#>$D800
	sta	ptr2+1
l4:	
	ldy	#0
	lda	#$5a
	sta	(ptr1),y
	lda	#3
	sta	(ptr2),y
	ldy	#39
	lda	#$5a
	sta	(ptr1),y
	lda	#3
	sta	(ptr2),y
	Yield
	lda	ptr1
	clc
	adc	#40
	sta	ptr1
	bcc	sk2
	inc	ptr1+1
sk2:
	lda	ptr2
	clc
	adc	#40
	sta	ptr2
	bcc	sk3
	inc	ptr2+1
sk3:
	lda	ptr1+1
	cmp	#>(SCREENBASE)+4
	bne	l4
	ldx	#t7d_end-t7d
l5:	lda	t7d,x
	sta	SCREENBASE+12+12*40,x
	dex
	bpl	l5
endl:	nop
	ldx	#20
l6:	Yield
	dex
	bpl	l6
	ldx	#0
l7:	lda	$D800+12+12*40,x
	eor	#3
	sta	$D800+12+12*40,x
	Yield
	inx
	cpx	#t7d_end-t7d
	bne	l7
	jmp	endl

