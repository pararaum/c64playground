	.include	"t7d/takeover_effects/bullets_to_right_bottom.i"
	.include	"t7d/libt7d.i"
	.include	"t7d/memoryfunctions.i"
	.include	"t7d/pseudo/yielding.i"
	.include	"t7d/pseudo/pointer.i"
	.include	"t7d/pseudo/pseudo16.inc"
	.include	"t7d/vic/vicmacros.i"
	.include	"t7d/petsciicopyframe.i"
	.include	"LAMAlib.inc"
	.include	"t7d/stringfunctions.i"
	.include	"zeropage.inc"
	.include	"consts.inc"
	.macpack	longbranch
	.macpack	cbm
	.import	_city_scene

	.export	CHARGEN_DESTINATION=$800
	.export	__SCREEN0_START__=$400
	.export	muzak_init=$ff6
	.export	muzak_play=$1003
	.export	PETSCIICOPY_SCREEN=$400


	.zeropage
scrollpointer:	.res	2

	.rodata
scrolltext:
	scrcode	" Like Ice in the Sunshine...  Turn the Joystick handle until you feel the Heat! Then you will melt like Ice in the Sunshine...     "
	scrcode "   Greetings melt to: "
	scrcode " Wizball6502, Commodore Treffen Graz, Trex, Delysid, Rebels, Joe, Quantum, The Solution, Hokuto Force, Gorgh, Abyss Connection, CPC User Club, Atlantis, Blazon, Excess, Gloegg, Haujobb, Padua, Laxity, Onslaught, Finnish Gold, Genesis Project, Triad, Nodepond, Digital Talk Team, Hharekiet, Rabenauge, Fairlight, Sissim, Cosmos Designs, and... everybody we forgot... "
	scrcode	"     Probably by now your joystick should be exterminated. If you like to read this scroller again without sacrifying another joystick then there is help. Before running the demo use 'POKE 55,5' or any non-zero value and this value will be ored to the background colour. "
	scrcode	"             "
	.byte	$ff
scrollspeed:	.byte	3

	.rodata
charanim0:
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
charanim1:
	.byte	%11111111
	.byte	%10000001
	.byte	%10000001
	.byte	%10000001
	.byte	%10000001
	.byte	%10000001
	.byte	%10000001
	.byte	%11111111
charanim2:
	.byte	%11111111
	.byte	%11111111
	.byte	%11000011
	.byte	%11000011
	.byte	%11000011
	.byte	%11000011
	.byte	%11111111
	.byte	%11111111
charanim3:
	.byte	%11111111
	.byte	%11111111
	.byte	%11011111
	.byte	%11101111
	.byte	%11110111
	.byte	%11111011
	.byte	%11111111
	.byte	%11111111
charanim4:
	.byte	%11111111
	.byte	%11111111
	.byte	%11111011
	.byte	%11110111
	.byte	%11101111
	.byte	%11011111
	.byte	%11111111
	.byte	%11111111
charanim5:
	.byte	%11111111
	.byte	%11111111
	.byte	%11111111
	.byte	%11100111
	.byte	%11100111
	.byte	%11111111
	.byte	%11111111
	.byte	%11111111
charanim6:
	.byte	%11111111
	.byte	%11111111
	.byte	%11000011
	.byte	%11000011
	.byte	%11000011
	.byte	%11000011
	.byte	%11111111
	.byte	%11111111
charanim7:
	.byte	%11111111
	.byte	%10000001
	.byte	%10000001
	.byte	%10000001
	.byte	%10000001
	.byte	%10000001
	.byte	%10000001
	.byte	%11111111

	.rodata
	.res	$800,$FF	; This should hit the ghost byte to mask out the background.
	
	.data
enabledfld:	.byte	0

	.data
orbackground:	.byte	0

	.data
framecounter:	.word	0
continueflag:	.byte	0

charbuffer:	.res	8*8
charbuffer_end:


	.segment	"EXEHDR"
	.res	$800,$ea
	ldax	#scrolltext
	stax	scrollpointer
	sei
	CopyFileIntoMemoryDown	muzak_init,"terminator_in_sunshine.sid",$7c+2
	jmp	_main

	.code
	.scope joyquirl
	.include	"m_joyquirl.s"
	.endscope


	.code
irq:	jsr	update_takeover_bulleteffect
	jcs	overtaking
	SetIRQ314Pointer	irq2
	EnableIRQatRasterline	INITIALIRQLINE
	lda	#0
	sta	$d011
	CopyFileIntoMemoryDown	$800,"Syncwave.both.64c",2	
overtaking:
	jmp	$ea31


.proc	irq2
	lda	$d019		; Is raster?
	and	#1
	bne	raster
	;; 	cli			;Muzak must be interruptible for FLD!
	jsr	muzak_play
	jmp	$ea31
raster:
	sta	$d019
	bit	$d011
	bmi	bottomirq
	lda	enabledfld
	beq	nofld
	jsr	fldirq
	lda	#<NORMALIRQLINE
	.byte	$2c		; Skip two-byte instruction.
nofld:	lda	#<INITIALIRQLINE
	sta	$d012
	lda	$d011
	ora	#$80
	sta	$d011
	jmp	$ea81
bottomirq:
	lda	ptr1
	pha
	lda	ptr1+1
	pha
	lda	tmp1
	pha
	CallYielding
	pla
	sta	tmp1
	pla
	sta	ptr1+1
	pla
	sta	ptr1
	inc16	framecounter
	jmp	$ea81
.endproc


.proc	yieldingirq
;;; https://www.krajzewicz.de/blog/stretching-the-c64-palette.php
	.macro	WaitFrames frms
	.local	@l1
	ldx	#frms
@l1:	Yield
	dex
	bne	@l1
	.endmacro
	WaitFrames	3
	lda	#12
	sta	$d020
	WaitFrames	3
	lda	#9
	sta	$d020
	WaitFrames	3
	lda	#8
	sta	$d020
	WaitFrames	3
	lda	#2
	sta	$d020
	WaitFrames	3
	lda	#4
	sta	$d020
	lda	#0
	sta	$d01a		; Disable the interrupts
	ldax	#_city_scene
	jsr	petsciicopyframe
	lda	#1
	sta	$d01a
	Yield
	lda	#$9b		; Remember that we are above 255 with our raster irq.
	sta	$d011
	lda	#$18
	sta	$d016		; Multicol mode.
	lda	#$b
	sta	$d022		; Multicol 1.
	lda	#$c
	sta	$d023		; Multicol 2.
	;; Enjoy the picture.
	WaitFrames	3*50
	WaitFrames	4*50
	.repeat	25,I
	SmallMemSet	0,40,$d800+40*I
	lda	#0
	.if	I>6
	 sta	$d022
	.endif
	.if	I>12
	 sta	$d020
	 sta	$d021
	.endif
	.if	I>18
	 sta	$d023
	.endif
	Yield
	.endrepeat
	lda	#$8
	sta	$d016		; No more multicol!
	inc	continueflag
stuckinprint:
	Yield
	lda	continueflag
	bne	stuckinprint
	ldx	#250
inverseloop:
	.repeat	4,I
	 lda	PETSCIICOPY_SCREEN+250*I-1,x
	 eor	#$80
	 sta	PETSCIICOPY_SCREEN+250*I-1,x
	.endrepeat
	dex
	bne	inverseloop
	Yield
	ldy	#40
flashloop:
	lda	#2
	sta	$d021
	WaitFrames 	2
	lda	#8
	sta	$d021
	WaitFrames	2
	lda	#10
	sta	$d021
	WaitFrames	2
	lda	#7
	sta	$d021
	WaitFrames	3
	lda	#10
	sta	$d021
	WaitFrames	2
	lda	#8
	sta	$d021
	WaitFrames	2
	lda	#2
	sta	$d021
	WaitFrames 	2
	dey
	bne	flashloop
	lda	#<(1000)
	pha
	lda	#>(1000)
	pha
	ldax	#$400
	ldy	#(' '|$80)
	jsr	memsetAX
QUICK:	lda	#5
	sta	$d021
	sta	enabledfld
	SmallMemSet	$ff,8,CHARGEN_DESTINATION
yloop:	Yield
	.pushseg
	.rodata
	.byte	2,4
colours:	.byte	9,7,7,1,1,1,1,1,1,1,1,1,1
colours_end:
	.data
oldquirl:	.byte	0
newquirl=$600
quirlcounter:	.byte	0
	.popseg
	lda	framecounter	; Current frame number
	and	#1		; Do the check every 8th frame
	if eq
		m_run	joyquirl
		if eq
			ora	orbackground
			sta	$d021
		else
;			inc	quirlcounter
;			ldx	quirlcounter
;			sta	newquirl,x
			pha
			sec
			sbc	oldquirl
			bpl	quirlok
			lda	#0
quirlok:
			tax
			pla
			sta	oldquirl
			lda	colours,x
			ora	orbackground
			sta	$d021
			endif
	endif
	lda	scrollvalue
	and	#7
	sec
	sbc	scrollspeed
	sta	scrollvalue
	bpl	no_overflow
	clc
	adc	#8
	sta	scrollvalue
	ldx	#0
scrollloop:
	.repeat	8,I
	lda	PETSCIICOPY_SCREEN+SCROLLLINE*40+I*40+1,x
	sta	PETSCIICOPY_SCREEN+SCROLLLINE*40+I*40,x
	.endrepeat
	inx
	cpx	#39
	bne	scrollloop
	jsr	next_char_row
	cpx	#8
	bne	no_overflow
	jsr	next_from_text
no_overflow:
	jsr	animate_chars
	lda	#FLDIRQLINE
	sta	$d012
	lda	#$1b
	sta	$d011
	lda	#8		; Default value.
	sta	$d016
	jmp	yloop
.endproc	

	.data
.proc	animate_chars
	lda	framecounter
	lsr
	lsr
	lsr
	and	#7
	on_A_jmp0	anim0,anim1,anim2,anim3,anim4,anim5,anim6,anim7
	rts
	ANIMDEST=CHARGEN_DESTINATION+$20*8
anim0:	SmallMemCpy	charanim0,8,ANIMDEST
	rts
anim1:	SmallMemCpy	charanim1,8,ANIMDEST
	rts
anim2:	SmallMemCpy	charanim2,8,ANIMDEST
	rts
anim3:	SmallMemCpy	charanim3,8,ANIMDEST
	rts
anim4:	SmallMemCpy	charanim4,8,ANIMDEST
	rts
anim5:	SmallMemCpy	charanim5,8,ANIMDEST
	rts
anim6:	SmallMemCpy	charanim6,8,ANIMDEST
	rts
anim7:	SmallMemCpy	charanim7,8,ANIMDEST
	rts
.endproc

	
	.code
.proc	next_char_row
	ldx	current
	.repeat	8,I
	lda	charbuffer+I*8,x
	sta	PETSCIICOPY_SCREEN+SCROLLLINE*40+I*40+39
	.endrepeat
	inx
	txa
	and	#7
	sta	current
	rts
current:	.byte	0
.endproc

	.code
.proc	next_from_text
	IncrementAndYLoad	scrollpointer
	cmp	#$FF		; Endmarker
	bne	notend
	ldax	#scrolltext
	stax	scrollpointer
	lda	#' '
notend:
	cmp	#' '		; Space?
	bne	nospace
	;; We can not use space as space is the animated character, therefore we quickly and quietly fill the buffer with the opaque element. Which is conveniently zero.
	jsr	clear_memory_progmem
	.word	charbuffer
	.word	charbuffer_end-charbuffer
	rts
nospace:
	sty	tmp1		;Y=0
	;; A=next char
	asl			; *2
	rol	tmp1
	asl			; *2
	rol	tmp1
	asl			; *2
	rol	tmp1
	ldx	tmp1		; AX=char*8
	addax	#CHARGEN_DESTINATION
	stax	ptr1
	ldax	#charbuffer
	stax	charbufferptr
rowloop:
	lda	(ptr1),y
	sta	tmp1
	ldx	#0
colloop:
	lda	#0
	asl	tmp1
	bcc	nobit
	ora	#$20
nobit:	sta	charbuffer,x
	charbufferptr=*-2
	inx
	cpx	#8
	bne	colloop
	P_add	#8,charbufferptr
	iny
	cpy	#8
	bne	rowloop
	rts
.endproc


_main:
	ldax	#next_from_text
	stax	$200
	lda	55		; Defaults to zero, LO($A000).
	sta	orbackground
	lda	#0
	jsr	muzak_init
	lda	$d020
	jsr	init_takeover_bulleteffect
	SetupYielding	yieldingirq
	SetIRQ314Pointer	irq
	cli
l1:	lda	continueflag
	beq	l1
	;;
	jsr	output_string_deluxe
	.byte	2, 2
	.byte	38,10
	.byte	$90,$93
	.byte	"Like Black",13
	.byte	"++++++++++",13,13
	.byte	$12,"Code: ",$92," Pararaum",13
	.byte	$12,"Code: ",$92," Wil",13
	.byte	$12,"Font: ",$92," GamienG",13
	.byte	$12,"Gfx:  ",$92," Pararaum",13
	.byte	$12,"Music:",$92," Nordischsound",13
	.byte	1,	1,14,39,10
	.byte	"A fun entry for the VCC$30 black on",13
	.byte	"black competition. The power has gone",13
	.byte	"out and in order to read the scroller,",13
	.byte	"turn the joystick in circles like a",13
	.byte	"dynamo. This will produce energy for",13
	.byte	"the background illumination!"
	brk
	;;
	lda	#0
	sta	continueflag
l2:	lda	continueflag
	beq	l2
	brk
