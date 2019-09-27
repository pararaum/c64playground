;;; https://codebase64.org/doku.php?id=base:4x4_mode_and_circle_routine
;;; xa -M graph4x4.asm

CHARSET=$2800+768

;4x4 pixel mode
;by malcontent in 2012
;XA assembler format
;
;This is a rather simple and fast way of 
;painting blocky pixels. To use it you 
;need to modify the character set so that 
;each 4x4 tile has a nibble that matches
;its pattern.
;
;example:
;
;  %xxxx0001 = 00	xxxx0110 = 01
;              01		   10	
;
;A single point than can be bitshifted into
;position based on whether division of the 
;co-ordinate leaves a carry. LSR once to 
;move the point to the right, and twice to
;move it down.
;
;Tables speed up the conversion of x y  
;co-ordinates to screen memory locations.
;
;
;Much like the kernal plot, the registers
;are reversed. On entry have the x point 
;in the y and put the y point in the x
;register.
;
;There is no check for out of screen points.
;Valid ranges: x:0-79 y:0-49

zp1 = $fb
zp2 = $fc
zx  = $fd
zy  = $fe
pnt = $fa

;Circle variables are signed 16-bit values

r   = $e0	;Radius
xc  = $e2	;X circle center
yc  = $e4	;Y  "   "
xp  = $e6	;X plot on circle edge
yp  = $e8	;Y  "   "
xd  = $ea	;Transformed point to draw
yd  = $ec	;
tx  = $ee
tmp = $f0	;Single byte

screendia = 93	;If the center of the 
		;circle is on the screen,
		;and the radius is larger 
		;than the screen diagonal,
		;then the circle does not 
		;need to be drawn.

blank = 96		;Offset to chars
			;Also blank char

        .word   $0801
        *=$0801

        .word   end_of_basic
        .word   main
        .byte   $9e,"2061"
end_of_basic:
        .byte   0,0,0

main:   sei
	lda	#$1b
	sta	$d018
	;; Copy charset.
	ldx	#blockchars_end-blockchars
ccl:	lda	blockchars,x
	sta	CHARSET,x
	dex
	bne	ccl
	ldx	#0
l1:	txa
	sta	$0400,x
	dex
	bne	l1
	jmp	INIT

blockchars:	.bin	0,0,"blockchars.bin"
blockchars_end:

INIT	
	jsr CLEARSCREEN
	
	lda #20
	sta xc
	sta yc
	lda #0
	sta xc+1
	sta r+1
	sta yc+1
	lda #5
	sta r
	
l107:	inc $d020
	jsr CIRCLE
	dec $d020
	inc xc
	inc r
	bne l107
	rts
	
PLOT 	lda #%00001000
	sta pnt
	tya
	lsr 
	bcc l121
	lsr pnt		;Point shifts right
l121:	tay		;if division has a
	txa		;remainder
	lsr
	bcc l127
	lsr pnt		;Point moves down
	lsr pnt		;on remainder
l127:	tax		

;x and y registers now hold 0-24,0-39
;so long as valid numbers went in.
;'pnt' contains the mask for the char.
			
	lda lotable,x  	;Table holds the
	sta zp1		;leftmost screen
	lda hitable,x	;address of row.
	sta zp2
	lda (zp1),y	;Get screen graphic
	ora pnt		;mask in point.
	sta (zp1),y
	rts
	
CLEARSCREEN	
	ldx #0
l144:	lda #blank
	sta $0400,x
	sta $0400+$ff,x
	sta $0400+$ff+$ff,x
	sta $0400+$ff+$ff+$ff,x
	lda #10
	sta $d800,x
	sta $d800+$ff,x
	sta $d800+$ff+$ff,x
	sta $d800+$ff+$ff+$ff,x
	inx
	bne l144
	rts
	
;-----------------------------------



plotcircle:		;Plot a circle point
	lda xd+1	;Is xy in screen?
	bne *+21	;If no, branch to    
	lda yd+1	;instruction after 
	bne *+17	;the macro.
	ldx yd
	cpx #50
	bcs *+11
	ldy xd
	cpy #80
	bcs *+5
	jsr PLOT	;Point is plotable
	rts
	
;Reject drawing the circle if the bounding
;box does not cross the screen.
;if x<0 and if x+r>0 then check y
;if x>0	and if x-r<screenwidth check y
	
	
nocir	rts
	
CIRCLE	lda xc+1
	bmi negxcen
	lda xc
	sec
	sbc r
	sta xd
	lda xc+1
	sbc r+1
	bmi checky
	bne nocir
	lda xd
	cmp #80
	bcc checky
	rts
negxcen	lda xc
	clc
	adc r
	sta xd
	lda xc+1
	adc r+1
	bmi nocir
checky	lda yc+1
	bmi negycen
	lda yc
	sec
	sbc r
	sta yd
	lda yc+1
	sbc r+1
	bmi bbok
	bne nocir
	lda yd
	cmp #50
	bcc bbok
	rts
negycen	lda yc
	clc
	adc r
	sta yd
	lda yc+1
	adc r+1
	bmi nocir
			
bbok	lda r		;Init radius and
	sta xp		;set first point
	sta tx		;to draw.
	lda r+1
	sta xp+1
	lsr
	sta tx+1	;tx=r/2
	ror tx
	lda #0
	sta yp
	sta yp+1
	
	;jmp cloop	;***************
	
	lda #$18	;clc
	sta radovfl	;Clear draw skips
	sta top
	sta topleft
	sta topright
	sta bottom
	sta bottomleft
	sta bottomright
	
;Some comparisons here modify the looping
;code that draws. Saves us from having to
;do the comparisons within the loop.
	
	ldx #$38	;sec
	ldy #$02	;decremented flag
	lda xc+1
	bmi drawr    	;x<0
	bne drawl   	;x>$ff
	lda xc
	cmp #80		;Screen width
	bcs drawl   	;x>80
	dey		;x in screen
	bne yarc
drawr   stx topleft
	stx bottomleft	;skip these
	jmp yarc
drawl	stx topright
	stx topright
	
yarc	lda yc+1
	bmi drawb    	;y<0
	bne drawt   	;y>$ff
	lda yc
	cmp #50		;Screen height
	bcs drawl   	;y>50
	dey		;y in screen
	bne cloop	
	lda r+1		;Center in screen.
	bne toobig	;Compare radius to
	lda r		;screen diagonal.
	cmp #screendia
	bcc cloop
toobig	rts
drawb   stx top    	;skip this
	jmp cloop
drawt	stx bottom

;All set up.

cloop	jsr plot8
	inc yp
	bne l293
	inc yp+1
l293	sec		;?
	lda tx
	sbc yp
	sta tx
	lda tx+1
	sbc yp+1
	sta tx+1
	bcs cloop
	dec xp
	bne l304
	dec xp+1
l304:	lda tx
	adc xp
	sta tx
	lda tx+1
	adc xp+1
	sta tx+1
	sec
	lda xp
	sbc yp
	sta tmp
	lda xp+1
	sbc yp+1
	ora tmp
	bcs cloop
	jsr plot8	;Get last bit
	rts

;plot8 is modified by the circle init
;routine to avoid drawing unnecessary 
;parts of the circle. It pokes SECs over
;CLCs to branch or fall through to the
;right transformations
;
;30 DRAW1,X+XO,Y+YO:DRAW1,Y+XO,X+YO
;40 DRAW1,XO-X,YO+Y:DRAW1,XO-Y,YO+X
;50 DRAW1,XO-X,YO-Y:DRAW1,XO-Y,YO-X
;60 DRAW1,XO+X,YO-Y:DRAW1,XO+Y,YO-X


noplot1	rts

plot8	
radovfl	clc		;Radius overflow
	bcs noplot1
top	clc		;clc draw top
	bcc topleft
	jmp bottom
topleft	clc
	bcs topright
	sec
	lda xc
	sbc xp
	sta xd
	lda xc+1
	sbc xp+1
	sta xd+1
	sec
	lda yc
	sbc yp
	sta yd
	lda yc+1
	sbc yp+1
	sta yd+1
	jsr plotcircle
	sec
	lda xc
	sbc yp
	sta xd
	lda xc+1
	sbc yp+1
	sta xd+1
	sec
	lda yc
	sbc xp
	sta yd
	lda yc+1
	sbc xp+1
	sta yd+1
	jsr plotcircle
topright
	clc		
	bcs bottom	
	lda xc
	adc xp
	sta xd
	lda xc+1
	adc xp+1
	sta xd+1
	sec
	lda yc
	sbc yp
	sta yd
	lda yc+1
	sbc yp+1
	sta yd+1
	jsr plotcircle
	clc
	lda xc
	adc yp
	sta xd
	lda xc+1
	adc yp+1
	sta xd+1
	sec
	lda yc
	sbc xp
	sta yd
	lda yc+1
	sbc xp+1
	sta yd+1
	jsr plotcircle	
bottom 	clc		;bottom half
	bcc bottomleft
	rts
bottomleft
	clc
	bcs bottomright
	sec
	lda xc
	sbc xp
	sta xd
	lda xc+1
	sbc xp+1
	sta xd+1
	clc
	lda yc
	adc yp
	sta yd
	lda yc+1
	adc yp+1
	sta yd+1
	jsr plotcircle
	sec
	lda xc
	sbc yp
	sta xd
	lda xc+1
	sbc yp+1
	sta xd+1
	clc
	lda yc
	adc xp
	sta yd
	lda yc+1
	adc xp+1
	sta yd+1
	jsr plotcircle
bottomright
	clc
	bcs noplot
	lda xp
	adc xc
	sta xd
	lda xp+1
	adc xc+1
	sta xd+1
	clc
	lda yp
	adc yc
	sta yd
	lda yp+1
	adc yc+1
	sta yd+1
	jsr plotcircle
	clc
	lda yp
	adc xc
	sta xd
	lda yp+1
	adc xc+1
	sta xd+1
	clc
	lda xp
	adc yc
	sta yd
	lda xp+1
	adc yc+1
	sta yd+1
	jsr plotcircle	
noplot	rts

	
lotable:	.byte $00,$28,$50,$78,$a0,$c8,$f0
	.byte $18,$40,$68,$90,$b8,$e0
	.byte $08,$30,$58,$80,$a8,$d0,$f8
	.byte $20,$48,$70,$98,$c0
	
hitable:	.byte $04,$04,$04,$04,$04,$04,$04
	.byte $05,$05,$05,$05,$05,$05
	.byte $06,$06,$06,$06,$06,$06,$06
	.byte $07,$07,$07,$07,$07


