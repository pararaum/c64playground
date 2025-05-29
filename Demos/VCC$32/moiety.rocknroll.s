	.include	"t7d/memoryfunctions.i"
	.include	"t7d/pseudo/loadstorereg.i"
	.include	"t7d/multiply.i"
	.include	"moiety.rocknroll.i"
	.include	"zeropage.inc"
	.include	"LAMAlib-macros16.inc"
	.include	"t7d/stackmacros.i"

WIDTHOFSOURCE=96

	.zeropage
lineptrs:	.res	25*2	; A pointer for each line


	.data
ROCKNROLL_XOFFSET:	.word	0 ; X offset for next frame.
ROCKNROLL_YOFFSET:	.word	0 ; Y offset for next frame.


	.bss
;;; The buffer selector is used to decide which screen to use: If bit 6 is...
;;; 0: Draw textscreen 0 but display 1
;;; 1: Draw textscreen 1 but display 0
bufferselector:	.res	1
nextD016:	.res	1	; $D016 in next frame.
nextD011:	.res	1	; $D011 in next frame.
initialxposptr:	.res	2	; Initial pointer for x positions.
initialyposptr:	.res	2	; Initial pointer for y positions.
currentxposptr:	.res	2
currentyposptr:	.res	2
sourcescrptr:	.res	2	; Pointer to the sourcescreen.

	.import xpositions

	.code
.proc	change_rocknroll
	sei
	stax	sourcescrptr

	ldax #22*8
	cpy #4
	bne not4
	ldax ROCKNROLL_XOFFSET
	cmpax #22*8
	bcc ok
	subax #22*8
	stax ROCKNROLL_XOFFSET
ok:
	ldax #$ffff
not4:
	stax xpositions+22*2*8

	cli
	rts
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
	PullStoreStackptrLOCAL
	lda	#%01000000
	sta	bufferselector
	lda	$d016
	sta	nextD016
	lda	$d011
	sta	nextD011
	;;
	pla
	sta	sourcescrptr+1
	pla
	sta	sourcescrptr
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
;;; Input: AX=pointer to the top-left corner, Y=source stride-width
;;; Modifies: A,X,Y,tmp1
.proc	plot0_at_AX
	sty	tmp1
	;; Setup one pointer per text line.
	.repeat	25,I
	 stax	lineptrs+2*I
	 clc
	 adc	tmp1
	 .scope
	  bcc	noXinc
	  inx
noXinc:
	 .endscope
	.endrepeat
	ldy	#40-1		; 40 columns.
loop:
	.repeat	25,I
	 lda	(lineptrs+2*I),y ; Get character.
	 sta	ROCKNROLL_TEXTSCREEN0+40*I,y
	.endrepeat
	dey
	bpl	loop
	rts
.endproc


;;; Draw into textscreen 1.
;;; Input: AX=pointer to the top-left corner, Y=source stride-width
;;; Modifies: A,X,Y,tmp1
.proc	plot1_at_AX
	sty	tmp1
	;; Setup one pointer per text line.
	.repeat	25,I
	 stax	lineptrs+2*I
	 clc
	 adc	tmp1
	 .scope
	  bcc	noXinc
	  inx
noXinc:
	 .endscope
	.endrepeat
	ldy	#40-1		; 40 columns.
loop:
	.repeat	25,I
	 lda	(lineptrs+2*I),y ; Get character.
	 sta	ROCKNROLL_TEXTSCREEN1+40*I,y
	.endrepeat
	dey
	bpl	loop
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
	ror
	;; y*96=y*64+y*32
	ldax	ROCKNROLL_YOFFSET
	and	#%11111000
	stax	ptr1		; Y*8
	asl	ptr1		; Y*16
	rol	ptr1+1
	asl	ptr1		; Y*32
	rol 	ptr1+1
	ldax	ptr1
	stax	ptr2
	asl	ptr1		; Y*64
	rol 	ptr1+1
	lda	ptr2
	clc
	adc	ptr1
	sta	ptr1
	lda	ptr2+1
	adc	ptr1+1
	sta	ptr1+1
	;; Get column offset.
	lda	ROCKNROLL_XOFFSET+1 ; HI of X offset.
	sta	tmp1
	lda	ROCKNROLL_XOFFSET
	lsr	tmp1		; Divide by 2
	ror
	lsr	tmp1		; Divide by 2
	ror
	lsr	tmp1		; Divide by 2
	ror
	clc
	adc	ptr1
	sta	ptr1
	lda	ptr1+1
	adc	tmp1
	sta	ptr1+1
	lda	ptr1
	clc
	adc	sourcescrptr
	sta	ptr1
	lda	ptr1+1
	adc	sourcescrptr+1
	sta	ptr1+1
	;; Prepare draw parameters.
	ldy	#WIDTHOFSOURCE
	ldax	ptr1
	;; Choose correct draw.
	bit	bufferselector
	bvc	screen0
	jmp	plot1_at_AX
screen0:
	jmp	plot0_at_AX
.endproc
