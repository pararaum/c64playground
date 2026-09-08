	.include	"t7d/memoryfunctions.i"
	.include	"t7d/pseudo/loadstorereg.i"
	.include	"t7d/multiply.i"
	.include	"moiety.rocknroll.i"
	.include	"zeropage.inc"
	.include	"LAMAlib-macros16.inc"
	.include	"t7d/stackmacros.i"
	.macpack	longbranch

MAXIMUMHEIGHT=50


	.data
ROCKNROLL_XOFFSET:	.word	0 ; X screen offset for next frame in pixel.
ROCKNROLL_YOFFSET:	.word	0 ; Y screen offset for next frame in pixel.
ROCKNROLL_XCELL:	.byte	0 ; X screen offset for next frame in cells.
ROCKNROLL_YCELL:	.byte	0 ; Y screen offset for next frame in cells.
nextD016:	.byte	0	; $D016 in next frame, defaults to 38 columns.
nextD011:	.byte	$10	; $D011 in next frame, defaults to text screen 24 rows.


	.bss
;;; The buffer selector is used to decide which screen to use: If bit 6 is...
;;; 0: Draw textscreen 0 but display 1
;;; 1: Draw textscreen 1 but display 0
bufferselector:	.res	1
initialxposptr:	.res	2	; Initial pointer for x positions.
initialyposptr:	.res	2	; Initial pointer for y positions.
currentxposptr:	.res	2
currentyposptr:	.res	2
sourcewidth:	.res	1	; Width of source screen in characters.
sourceheight:	.res	1	; Height of source screen in characters.
;;; Pointers to the lines of the source screen.
sourcelineptrLO:	.res	MAXIMUMHEIGHT	; Maximum number of lines.
sourcelineptrHI:	.res	MAXIMUMHEIGHT	; Maximum number of lines.

	.code
.proc	change_rocknroll
	sta	sourcelineptrLO	; Set the first LO/HI.
	txa
	sta	sourcelineptrHI
	ldx	#0		; Start at the beginning of lines.
notdone:
	lda	sourcelineptrLO,x
	clc
	adc	sourcewidth
	sta	sourcelineptrLO+1,x
	lda	sourcelineptrHI,x
	adc	#0
	sta	sourcelineptrHI+1,x
	inx
	cpx	#MAXIMUMHEIGHT-1
	bne	notdone
	rts
.endproc


.proc	reinit_rocknroll_Xpos
	sei
	stax	initialxposptr
	cli
	rts
.endproc

.proc	reinit_rocknroll_Ypos
	sei
	stax	initialyposptr
	cli
.endproc


.proc	change_rocknroll_xpos
	sei
	stax	initialxposptr
	stax	currentxposptr
	cli
	rts
.endproc

.proc	change_rocknroll_ypos
	sei
	stax	initialyposptr
	stax	currentyposptr
	cli
	rts
.endproc

	.code
.proc	init_rocknroll
	stx	sourcewidth	; Store selected source width.
	sty	sourceheight	; Store selected source height (not used for now).
	PullStoreStackptrLOCAL
	lda	#%01000000
	sta	bufferselector
	;;
	pla
	tax			; HI in X.
	pla			; LO in A.
	jsr	change_rocknroll
	;;
	pla
	sta	initialyposptr+1
	sta	currentyposptr+1
	pla
	sta	initialyposptr
	sta	currentyposptr
	;;
	pla
	sta	initialxposptr+1
	sta	currentxposptr+1
	pla
	sta	initialxposptr
	sta	currentxposptr
	RetrievePushStackptrLOCAL
	rts
.endproc

.proc	update_rocknroll
	ldax	currentxposptr
doXrestart:
	stax	ptr1
	ldy	#0
	lda	(ptr1),y
	sta	ROCKNROLL_XOFFSET
	iny
	lda	(ptr1),y
	sta	ROCKNROLL_XOFFSET+1
	bpl	noXrestart
	ldax	initialxposptr
	stax	currentxposptr
	jmp	doXrestart
noXrestart:
	lda	currentxposptr
	clc
	adc	#2
	sta	currentxposptr
	bcc	noxcarry
	inc	currentxposptr+1
noxcarry:
	;;
	ldax	currentyposptr
doYrestart:
	stax	ptr1
	ldy	#0
	lda	(ptr1),y
	sta	ROCKNROLL_YOFFSET
	iny
	lda	(ptr1),y
	sta	ROCKNROLL_YOFFSET+1
	bpl	noYrestart
	ldax	initialyposptr
	stax	currentyposptr
	jmp	doYrestart
noYrestart:
	lda	currentyposptr
	clc
	adc	#2
	sta	currentyposptr
	bcc	noycarry
	inc	currentyposptr+1
noycarry:
	;;
	lda	bufferselector
	eor	#$40
	sta	bufferselector
	rts
.endproc


;;; Draw into textscreen 0.
;;; Input: X=column, Y=row
;;; Modifies: A,X,Y,tmp1
.proc	plot0_at_XY
	;; Setup one pointer per text line.
	.repeat	25,I
	lda	sourcelineptrLO,y
	sta	loop+1+6*I
	lda	sourcelineptrHI,y
	sta	loop+1+6*I+1
	iny
	.endrepeat
	;; Actual copy operation:
	ldy	#40-1		; Counter for 40 columns in Y.
	txa			; Add 40 columns to counter in X, we copy from right to left.
	clc
	adc	#40-1
	tax
loop:				; Loop begins here, see above.
	;; 25 times the address of the beginning of the line must be overwritten.
	.repeat	25,I
	 lda	$BDBD,x 	; Get character, address is set above.
	 sta	ROCKNROLL_TEXTSCREEN0+40*I,y
	.endrepeat
	dex			; Next (actually previous) column.
	dey			; Decrement counter.
	jpl	loop
	rts
.endproc


;;; Draw into textscreen 1.
;;; Input: X=column, Y=row
;;; Modifies: A,X,Y,tmp1
.proc	plot1_at_XY
	;; Setup one pointer per text line.
	.repeat	25,I
	lda	sourcelineptrLO,y
	sta	loop+1+6*I
	lda	sourcelineptrHI,y
	sta	loop+1+6*I+1
	iny
	.endrepeat
	;; Actual copy operation:
	ldy	#40-1		; Counter for 40 columns in Y.
	txa			; Add 40 columns to counter in X, we copy from right to left.
	clc
	adc	#40-1
	tax
loop:				; Loop begins here, see above.
	;; 25 times the address of the beginning of the line must be overwritten.
	.repeat	25,I
	 lda	$BDBD,x 	; Get character, address is set above.
	 sta	ROCKNROLL_TEXTSCREEN1+40*I,y
	.endrepeat
	dex			; Next (actually previous) column.
	dey			; Decrement counter.
	jpl	loop
	rts
.endproc


;;; Update the vic registers for this frame, was next frame last frame.
.proc 	update_vic
	bit	bufferselector
	bvc	display1
	lda	$d018		  ; Screen memory register.
	and	#%00001111	  ; Clear.
	ora	#<((ROCKNROLL_TEXTSCREEN0/$400)<<4)
	sta	$d018
	jmp	continue
display1:
	lda	$d018		  ; Screen memory register.
	and	#%00001111	  ; Clear.
	ora	#<((ROCKNROLL_TEXTSCREEN1/$400)<<4)
	sta	$d018
continue:
	lda	nextD016
	sta	$d016
	lda	nextD011
	sta	$d011
	rts
.endproc

.proc	calc_vic_nextframe
	lda	ROCKNROLL_XOFFSET ; X-offset.
	and	#7		  ; Lowest three bits.
	;; 7-A
	eor	#$FF
	sec
	adc	#7
	sta	tmp1
	lda	$d016		; Get screen control register #2.
	and	#%11111000	; Set lowest three bits to zero, so that...
	ora	tmp1		; ...the scroll bits calculated from the x-offset can be OR'ed.
	sta	nextD016
	lda	ROCKNROLL_YOFFSET ; Y-offset.
	and	#7
	eor	#$FF
	sec
	adc	#7
	sta	tmp1
	lda	$d011		; Get screen control register #1.
	and	#%11111000	; Clear scroll bits.
	ora	tmp1
	sta	nextD011
	rts
.endproc


	.code
.proc	draw_rocknroll
	jsr	update_vic
	jsr	calc_vic_nextframe
	;; Get line offset.
	lda	ROCKNROLL_YOFFSET+1 ; HI of Y offset.
	sta	tmp1
	lda	ROCKNROLL_YOFFSET
	lsr	tmp1		; Divide by 2
	ror
	lsr	tmp1		; Divide by 2
	ror
	lsr	tmp1		; Divide by 2
	ror			; Row number in A.
	tay			; Put row in Y.
	sta	ROCKNROLL_YCELL
	;; Get column offset.
	lda	ROCKNROLL_XOFFSET+1 ; HI of X offset.
	sta	tmp1
	lda	ROCKNROLL_XOFFSET
	lsr	tmp1		; Divide by 2
	ror
	lsr	tmp1		; Divide by 2
	ror
	lsr	tmp1		; Divide by 2
	ror			; Starting column in A.
	tax			; Put column in X.
	sta	ROCKNROLL_XCELL
	;; Choose correct draw.
	bit	bufferselector
	bvc	screen0
	jmp	plot1_at_XY
screen0:
	jmp	plot0_at_XY
.endproc
