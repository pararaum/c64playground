	.include	"zeropage.inc"
	.include	"globals.i"
	.import	_Colourball_Table

	.export	draw_colourballs

	.code
;;; Draw the colourballs at position X and Y
;;; Input: A=Y, X=X, Y=0 draw, bit 7 set=clear
.proc	draw_colourballs
	stx	tmp1		; tmp1 = x-positon
	sty	tmp2		; tmp2 = flag if draw or clear
	;; First advance to the correct row into the draw table.
	ldy	#0		; Y=0 for indexing.
	sty	ptr1+1		; ...and conveniently clear HI of pointer.
	.repeat 5		; Multiply by 32
	 asl
	 rol	ptr1+1
	.endrepeat
	clc
	adc	#<_Colourball_Table
	sta	ptr1
	lda	ptr1+1
	adc	#>_Colourball_Table
	sta	ptr1+1
	;; Loop for all 25 screen rows.
	.repeat	25,SCREENROW
	ldy	#0		; Start at first element.
	.scope
columnloop:
	lda	(ptr1),y	; Get the next colour.
	bmi	breakrow	; Negativ, we are done for this row.
	bit	tmp2		; Clear mode?
	bpl	noclearmode
	lda	#RNR_textcolour
noclearmode:
	sta	COLOUR		; Store colour for later use.
	iny
	lda	(ptr1),y	; Absolute Column in the picture.
	sec
	sbc	tmp1		; Subtract X offset.
	bmi	skipcell	; Left of the physical display.
	cmp	#40-1
	bcs	breakrow	; Right of the physical display.
	tax			; Shifted column into X
	lda	#0
	COLOUR=*-1		; Set above.
	sta	$d800+40*SCREENROW,x
skipcell:
	iny
	bne	columnloop
breakrow:
	.endscope
	lda	#2*16
	clc
	adc	ptr1
	sta	ptr1
	lda	ptr1+1
	adc	#0
	sta	ptr1+1
	.endrepeat
	rts
.endproc
