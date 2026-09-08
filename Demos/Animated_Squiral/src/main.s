	.include	"t7d/libt7d.i"
	.include	"t7d/memoryconfig.i"
	.include	"t7d/memoryfunctions.i"
	.include	"t7d/vic/vicmacros.i"
	.include	"t7d/stackmacros.i"
	.include	"t7d/frameengine.i"
	.include	"globals.i"
	.include        "moiety.rocknroll.i"
	.include        "moiety.wiggle.i"
	.include	"LAMAlib.inc"
	.include	"zeropage.inc"
	.include	"moiety.colourballs.i"
	.include	"moiety.smiley.i"
	.include	"moiety.greetings.i"
	.include	"moiety.smoking_hot.i"
	.include	"decrunch-lzp.eor.i"
	.include	"moiety.arc.i"

	.import	_frame0000		; The image to display and rocknroll.
	.import	_analyse_colourballs

QAD_RASTERSKIP=140

	.import	_main_squiral_fader

	.export	ROCKNROLL_TEXTSCREEN0=RNR_screen0
	.export	ROCKNROLL_TEXTSCREEN1=RNR_screen1

	.export	WIGGLE_DESTINATION_CHARSET=RNR_fontaddress
NO_CHARS_TO_WIGGLE=7
WIGGLE_EVERY_NTH=5

	.rodata
guitar_heroine:	.incbin	"guitar-heroine.raw"
credits_screen:	.incbin	"credits-screen.raw"
skeleton_heroine:	.incbin	"skeleton-heroine.raw"
	;; Include the colour byte.
guitar_heroine_colours:	.incbin	"guitar-heroine.prg",8000+2,1
guitar_heroine_animh0:	.incbin	"temporary.electricity.h0.lzp"
guitar_heroine_anim01:	.incbin	"temporary.electricity.01.lzp"
guitar_heroine_anim12:	.incbin	"temporary.electricity.12.lzp"
guitar_heroine_anim23:	.incbin	"temporary.electricity.23.lzp"
guitar_heroine_anim34:	.incbin	"temporary.electricity.34.lzp"
guitar_heroine_anim45:	.incbin	"temporary.electricity.45.lzp"

smiley_sprites:	.incbin	"smiley-sprites.4X4.2000.raw"
greetings_font:	.incbin	"Around.tumbled.raw"


	.rodata
WIGGLE_CHARS:
	.byte	64		; horizontal line
	.byte	68		; horizontal line, one row higher
	.byte	70		; horizontal line, one row lower
	.byte	66		; vertical line
	.byte	71		; vertical line, one column left
	.byte	72		; vertical line, one column left
	.byte	81		; the ball
WIGGLE_CHARSET:
	.incbin	"assets/wiggle-charset.64c",2,2*16*8

	.define	Wiggle_list	WIGGLE_CHARSET,WIGGLE_CHARSET+4*8,WIGGLE_CHARSET+8*8,WIGGLE_CHARSET+16*8,WIGGLE_CHARSET+20*8,WIGGLE_CHARSET+24*8,WIGGLE_CHARSET+12*8
WIGGLE_SOURCE_LO:
	.lobytes	Wiggle_list
WIGGLE_SOURCE_HI:	
	.hibytes	Wiggle_list

        .rodata
norocknrollposX:
	.word	0,0,0,0,1,1,1,1
	.word	$FFFF
norocknrollposY:
	.word	0,0,0,0,0,1,1,1,1,1
	.word	$FFFF


positions200_cos_8_6_6:
	;; l=200;','.join("%d"%(8*(9+8.9*math.cos(2*math.pi*p/float(l)+math.pi))-1) for p in range(l+1))
	.word	0,0,0,0,0,0,1,1,2,2,3,4,4,5,6,7,8,9,10,12,13,14,16,17,19,20,22,23,25,27,29,30,32,34,36,38,40,42,44,46,48,51,53,55,57,59,62,64,66,68,70,73,75,77,79,82,84,86,88,90,93,95,97,99,101,103,105,107,109,111,112,114,116,118,119,121,122,124,125,127,128,129,131,132,133,134,135,136,137,137,138,139,139,140,140,141,141,141,142,142,142,142,142,141,141,141,140,140,139,139,138,137,137,136,135,134,133,132,131,129,128,127,125,124,122,121,119,118,116,114,112,111,109,107,105,103,101,99,97,95,93,90,88,86,84,82,79,77,75,73,71,68,66,64,62,59,57,55,53,51,48,46,44,42,40,38,36,34,32,30,29,27,25,23,22,20,19,17,16,14,13,12,10,9,8,7,6,5,4,4,3,2,2,1,1,0,0,0,0,0,0
        .word   $FFFF

positions190_cos_8_6_6:
	;; l=190;','.join("%d"%(8*(9+8.9*math.cos(2*math.pi*p/float(l)+math.pi))-1) for p in range(l+1))
	.word	0,0,0,0,0,0,1,1,2,2,3,4,5,6,7,8,9,10,12,13,14,16,17,19,21,22,24,26,28,30,32,34,36,38,40,42,44,46,48,51,53,55,58,60,62,65,67,69,72,74,76,79,81,83,86,88,90,93,95,97,99,101,103,105,107,109,111,113,115,117,119,120,122,124,125,127,128,129,131,132,133,134,135,136,137,138,139,139,140,140,141,141,141,142,142,142,142,142,141,141,141,140,140,139,139,138,137,136,135,134,133,132,131,129,128,127,125,124,122,120,119,117,115,113,111,109,107,105,103,101,99,97,95,93,90,88,86,83,81,79,76,74,72,69,67,65,62,60,58,55,53,51,48,46,44,42,40,38,36,34,32,30,28,26,24,22,21,19,17,16,14,13,12,10,9,8,7,6,5,4,3,2,2,1,1,0,0,0,0,0,0
        .word   $FFFF

positions100_cos_8_6_6:
	;; l=100;','.join("%d"%(8*(9+8.9*math.cos(2*math.pi*p/float(l)+math.pi))-1) for p in range(l+1))
	.word	0,0,0,1,2,3,4,6,8,10,13,16,19,22,25,29,32,36,40,44,48,53,57,62,66,70,75,79,84,88,93,97,101,105,109,112,116,119,122,125,128,131,133,135,137,138,139,140,141,142,142,142,141,140,139,138,137,135,133,131,128,125,122,119,116,112,109,105,101,97,93,88,84,79,75,71,66,62,57,53,48,44,40,36,32,29,25,22,19,16,13,10,8,6,4,3,2,1,0,0,0
	.word	$FFFF

positions50_cos_8_6_6:
	;; l=50;','.join("%d"%(8*(9+8.9*math.cos(2*math.pi*p/float(l)+math.pi))-1) for p in range(l+1))
	.word	0,0,2,4,8,13,19,25,32,40,48,57,66,75,84,93,101,109,116,122,128,133,137,139,141,142,141,139,137,133,128,122,116,109,101,93,84,75,66,57,48,40,32,25,19,13,8,4,2,0,0
	.word	$FFFF

positions40_cos_8_6_6:
	;; l=40;','.join("%d"%(8*(9+8.9*math.cos(2*math.pi*p/float(l)+math.pi))-1) for p in range(l+1))
	.word	0,0,3,7,13,20,29,38,48,59,70,82,93,103,112,121,128,134,138,141,142,141,138,134,128,121,112,103,93,82,71,59,48,38,29,20,13,7,3,0,0
	.word	$FFFF

	
positions_sin8_6_6:
        ;;  ','.join("%d"%(8*(6+5.9*math.sin(2*math.pi*p/100.0+math.pi*math.sqrt(2)))-1) for p in range(100))
        .word   1,0,0,0,0,0,0,0,1,1,2,3,5,6,8,10,12,14,16,18,20,23,26,28,31,34,37,40,43,46,49,52,55,57,60,63,66,68,71,74,76,78,80,82,84,86,87,89,90,91,92,93,93,94,94,94,93,93,92,92,91,90,88,87,85,83,81,79,77,75,73,70,67,65,62,59,56,53,50,47,44,41,38,36,33,30,27,25,22,19,17,15,13,11,9,7,6,4,3,2
        .word   $FFFF

positions_50_sin8_6_6:
	.word	1,0,0,0,1,2,5,8,12,16,20,26,31,37,43,49,55,60,66,71,76,80,84,87,90,92,93,94,93,92,91,88,85,81,77,73,67,62,56,50,44,38,33,27,22,17,13,9,6,3
        .word   $FFFF

	.rodata
rauch:	.incbin	"datasrc.rauch.pu",2 ; Skip the pseudo load address...

	.zeropage
XIPZSRCPTR:	.res	2
XIPZDSTPTR:	.res	2
XIPZAUXPTR:	.res	2
	
	.segment	"EXEHDR"
	sei
	jsr	_disable_cia_irq
	memoryconfig_ram
	jsr	copy_stuff
	memoryconfig_io
	jsr	_main
	memoryconfig_kernal
	jmp	64738

copy_stuff:
	PushWordLH	#1000
	ldax	#GFX_hiscreenaddr
	ldy	guitar_heroine_colours
	jsr	memsetAX
	lda	#1		; Guitar Heroine
	jsr	decrunch_next_image
	;; Use one of Damien's nice fonts.
	CopyFileIntoMemoryDown RNR_fontaddress, "assets/Around.upper.64c", 2
	.assert * > $1000, error, "No space for copying down..."
	;; 	CopyFileIntoMemoryDown $1000, "assets/Dark_future.sid", $7c+2
	CopyFileIntoMemoryDown $1000, "assets/KillBill.sid", $7c+2+$1000-$ff6
	rts


	.code
init:	
	lda	#1
	jsr	$1000
	SwitchScreenAndChargenAddress GFX_hiscreenaddr, GFX_bitmapaddr
	lda	#0
	sta	$d012
	sta	$d011
	lda	#GFX_bordercolour
	sta	$d020
	lda	#RNR_textcolour
	jsr	_fill_colour_ram
	lda	#1
	sta	$d021
	SetIRQCPUPointer	feirqroutine
	EnableIRQatRasterline	0
	rts


	.code
.proc main_rockNroll
	lda	ROCKNROLL_YCELL
	sta	old_ROCKNROLL_YCELL
	lda	ROCKNROLL_XCELL
	sta	old_ROCKNROLL_XCELL
	jsr	draw_rocknroll
	jsr	update_rocknroll
	jsr	$1003
	memoryconfig_ram
	do_every	WIGGLE_EVERY_NTH
	lda	#NO_CHARS_TO_WIGGLE		; Number of characters to wiggle.
	jsr	update_wiggle
	end_every
	memoryconfig_io
	lda	#0
	old_ROCKNROLL_YCELL=*-1
	ldx	#0
	old_ROCKNROLL_XCELL=*-1
	ldy	#$FF
	jsr	draw_colourballs
	lda	ROCKNROLL_YCELL
	ldx	ROCKNROLL_XCELL
	ldy	#0
	jsr	draw_colourballs
	lda	#RNR_bordercolour
	rts
.endproc

	.code
	.include	"t7d/compression/xipz-qadz.decrunch.inc"

	.code
.proc	feirqroutine
	PushRegs
	lda	1		; Old memory config.
	pha
	lda	ptr1		;ptr1 may be used during interrupt, therefore store.
	pha
	lda	ptr1+1
	pha
	memoryconfig_io
	asl	$d019
muzak:	jsr	$1003
	jsr	frameengine_5c_allregs_run
	pla
	sta	ptr1+1
	pla
	sta	ptr1
	pla
	sta	1
	PullRegs
	rti
.endproc
	.export	IRQMUZAKCALL=feirqroutine::muzak

	.code
.proc	music_w_wait
	adc	$d012
l1:	cmp	$d012
	bcs	l1
	jmp	$1003
.endproc

;;; A=new border colour
.proc	set_border
	sta	$d020
	rts
.endproc


.proc	switch2hires
	SwitchScreenAndChargenAddress ::GFX_hiscreenaddr, ::GFX_bitmapaddr
	lda	#$3b
	sta	$d011		; Hires Graphics
	lda	#$8
	sta	$d016		; No multicolour, 40 columns, no scroll
	lda	#GFX_bordercolour
	sta	$d020
	rts
.endproc

.proc	switch2text
	lda	#$1b
	sta	$d011
	SwitchScreenAndChargenAddress $c800, $d000
	lda	#RNR_bordercolour
	sta	$d020
	rts
.endproc


;;; A=fill colour for text screen
.proc	fill_under_IO
	tay			; Move colour to Y
	ldx	#40-1
l1:	sei
	memoryconfig_ram
	tya
	.repeat	25,I
	  sta	GFX_hiscreenaddr+I*40,x
	.endrepeat
	memoryconfig_io
	cli
	dex
	bne	l1
	rts
.endproc


.proc	decrunch_next_image
	switch A
	case 1
	 ldax	#guitar_heroine
	 stax	XIPZSRCPTR
	 ldax	#GFX_bitmapaddr
	 stax	XIPZDSTPTR
	 jsr	decrunch_qadz
	break
	case 2
	;; Now decrunch the second image.
	 ldax	#credits_screen
	 stax	XIPZSRCPTR
	 ldax	#GFX_bitmapaddr
	 stax	XIPZDSTPTR
	 jsr	decrunch_qadz
	break
	case 3
	;; Now decrunch the third image.
	 ldax	#skeleton_heroine
	 stax	XIPZSRCPTR
	 ldax	#GFX_bitmapaddr
	 stax	XIPZDSTPTR
	 jsr	decrunch_qadz
	break
	default
	.byte	2
	endswitch
	rts
.endproc

.proc	decrunch_smiley_sprites
	ldax	#smiley_sprites
	stax	XIPZSRCPTR
	ldax	#GFX_bitmapaddr
	stax	XIPZDSTPTR
	jmp	decrunch_qadz
.endproc

.proc	decrunch_greetings_font
	ldax	#greetings_font
	stax	XIPZSRCPTR
	ldax	#GFX_bitmapaddr
	stax	XIPZDSTPTR
	jmp	decrunch_qadz
.endproc

	.data
eventtable:
	;; Remember to use zero for simultaneous actions in channels!
	;; Setup music and switch to Hires.
	;; Temp
	FrameJob5CAllRegsEntry	0,0,switch2text,0
	FrameJob5CAllRegsEntry	0,3,main_rockNroll,0
	
	FrameJob5CAllRegsEntry	400,0,switch2hires,0
	FrameJob5CAllRegsEntry	0,3,music_w_wait,QAD_RASTERSKIP

	;; 18.5s
	FrameJob5CAllRegsEntry	525,0,switch2text,0
	FrameJob5CAllRegsEntry	0,3,main_rockNroll,0
	FrameJob5CAllRegsEntry	0,1,reinit_rocknroll_Xpos,positions200_cos_8_6_6
	FrameJob5CAllRegsEntry	1,1,reinit_rocknroll_Ypos,positions190_cos_8_6_6


	;; 28s
	FrameJob5CAllRegsEntry	200,0,switch2hires,0
	FrameJob5CAllRegsEntry	0,3,music_w_wait,QAD_RASTERSKIP
	;; +5s
	FrameJob5CAllRegsEntry	250,0,switch2text,0
	;; New X positions
	FrameJob5CAllRegsEntry	0,1,reinit_rocknroll_Xpos,positions100_cos_8_6_6
	FrameJob5CAllRegsEntry	0,3,main_rockNroll,0

	;; Greetings
	FrameJob5CAllRegsEntry	100,4,fill_1000_bytes,Greetings_screen,$20
	FrameJob5CAllRegsEntry	100,4,decrunch_greetings_font,0
	FrameJob5CAllRegsEntry	200,0,pokeAXcommaY,$d011,0 ; Blank Screen
	FrameJob5CAllRegsEntry	2,0,init_greetings,0
	FrameJob5CAllRegsEntry	0,4,fill_1000_bytes,$d800,RNR_bordercolour
	FrameJob5CAllRegsEntry	0,2,update_greetings,0
	FrameJob5CAllRegsEntry	0,3,music_w_wait,QAD_RASTERSKIP
	;; Wait until done!
	FrameJob5CAllRegsEntry	1450,0,switch2text,0
	FrameJob5CAllRegsEntry	0,1,pokeAXcommaY,$d015,0 ; Sprites off!
	FrameJob5CAllRegsEntry	0,2,0,0
	FrameJob5CAllRegsEntry	0,3,main_rockNroll,0

	;; New Y positions
	FrameJob5CAllRegsEntry  100,0,reinit_rocknroll_Ypos,positions_sin8_6_6
	;; Decrunch next image, this is invisible as we switched to text.
	FrameJob5CAllRegsEntry	0,4,decrunch_next_image,2

	;; Display Hires Credits:
	FrameJob5CAllRegsEntry	200,0,switch2hires,0
	FrameJob5CAllRegsEntry	0,3,music_w_wait,QAD_RASTERSKIP

	;; Decrunch while in text mode otherwise the bitmap is overwritten...
	FrameJob5CAllRegsEntry	250,0,switch2text,0
	FrameJob5CAllRegsEntry	0,3,main_rockNroll,0
	;;
	FrameJob5CAllRegsEntry	100,4,decrunch_smiley_sprites,0 ; Decompress sprites.
	FrameJob5CAllRegsEntry	125,0,set_border,0 ; Border to black
	FrameJob5CAllRegsEntry	0,1,smiley_init,0  ; And on second channel initialise and run simley.
	;; From here on the smiley multiplexer takes over, after it returns the channels will be used as they were before...

	FrameJob5CAllRegsEntry  100,0,reinit_rocknroll_Ypos,positions_50_sin8_6_6
	;; Decrunch first image, again.
	FrameJob5CAllRegsEntry	0,4,decrunch_next_image,1
	FrameJob5CAllRegsEntry  150,0,reinit_rocknroll_Xpos,positions_50_sin8_6_6

	;; Graphics, again.
	FrameJob5CAllRegsEntry	200,0,switch2hires,0
	FrameJob5CAllRegsEntry	0,3,music_w_wait,QAD_RASTERSKIP

	
	;; 🩶🩶🩶🩶🩶🩶🩶🩶🩶🩶🩶🩶🩶🩶🩶🩶🩶🩶🩶🩶🩶🩶🩶🩶🩶🩶🩶🩶🩶🩶🩶🩶🩶🩶
	FrameJob5CAllRegsEntry	100,0,switch2text,0
	FrameJob5CAllRegsEntry	0,3,main_rockNroll,0
	FrameJob5CAllRegsEntry	0,4,decrunchLZP_to_E000,guitar_heroine_animh0
	FrameJob5CAllRegsEntry	100,0,switch2hires,0
	FrameJob5CAllRegsEntry	0,3,music_w_wait,QAD_RASTERSKIP
	FrameJob5CAllRegsEntry	100,0,switch2text,0
	FrameJob5CAllRegsEntry	0,3,main_rockNroll,0
	FrameJob5CAllRegsEntry	0,4,decrunchLZP_to_E000,guitar_heroine_anim01
	FrameJob5CAllRegsEntry	100,0,switch2hires,0
	FrameJob5CAllRegsEntry	0,3,music_w_wait,QAD_RASTERSKIP
	FrameJob5CAllRegsEntry	100,0,switch2text,0
	FrameJob5CAllRegsEntry	0,3,main_rockNroll,0
	FrameJob5CAllRegsEntry	0,4,decrunchLZP_to_E000,guitar_heroine_anim12
	FrameJob5CAllRegsEntry	100,0,switch2hires,0
	FrameJob5CAllRegsEntry  0,1,reinit_rocknroll_Xpos,positions40_cos_8_6_6
	FrameJob5CAllRegsEntry	0,3,music_w_wait,QAD_RASTERSKIP
	FrameJob5CAllRegsEntry	100,0,switch2text,0
	FrameJob5CAllRegsEntry	0,3,main_rockNroll,0
	FrameJob5CAllRegsEntry	0,4,decrunchLZP_to_E000,guitar_heroine_anim23
	FrameJob5CAllRegsEntry	100,0,switch2hires,0
	FrameJob5CAllRegsEntry	0,3,music_w_wait,QAD_RASTERSKIP
	FrameJob5CAllRegsEntry	100,0,switch2text,0
	FrameJob5CAllRegsEntry	0,3,main_rockNroll,0
	FrameJob5CAllRegsEntry	0,4,decrunchLZP_to_E000,guitar_heroine_anim34
	FrameJob5CAllRegsEntry	100,0,switch2hires,0
	FrameJob5CAllRegsEntry	0,3,music_w_wait,QAD_RASTERSKIP
	FrameJob5CAllRegsEntry	100,0,switch2text,0
	FrameJob5CAllRegsEntry	0,3,main_rockNroll,0
	FrameJob5CAllRegsEntry	0,4,decrunchLZP_to_E000,guitar_heroine_anim45
	FrameJob5CAllRegsEntry	100,0,switch2hires,0
	FrameJob5CAllRegsEntry	0,3,music_w_wait,QAD_RASTERSKIP

	;FrameJob5CAllRegsEntry	200,4,decrunchLZP_to_E000,guitar_heroine_anim01
	;FrameJob5CAllRegsEntry	200,4,decrunchLZP_to_E000,guitar_heroine_anim12
	;FrameJob5CAllRegsEntry	200,4,decrunchLZP_to_E000,guitar_heroine_anim23
	;FrameJob5CAllRegsEntry	200,4,decrunchLZP_to_E000,guitar_heroine_anim34
	;FrameJob5CAllRegsEntry	200,4,decrunchLZP_to_E000,guitar_heroine_anim45

	FrameJob5CAllRegsEntry	200,4,arc_init
	FrameJob5CAllRegsEntry	0,2,0
	FrameJob5CAllRegsEntry	0,3,0
	FrameJob5CAllRegsEntry	5,2,arc_run

	;; Decrunch while in text mode otherwise the bitmap is overwritten...
	FrameJob5CAllRegsEntry	100,0,switch2text,0 ; Text
	FrameJob5CAllRegsEntry	0,1,pokeAXcommaY,IRQMUZAKCALL,$20 ; Enable Muzak again.
	FrameJob5CAllRegsEntry	0,2,0				  ; Disable SFX.
	FrameJob5CAllRegsEntry	0,3,main_rockNroll
	FrameJob5CAllRegsEntry	0,4,decrunch_next_image,3 ; And decrunch.

	;; Switch to hires to display the image.
	FrameJob5CAllRegsEntry	200,3,music_w_wait,QAD_RASTERSKIP
	FrameJob5CAllRegsEntry	0,4,pudecrunch_default,rauch
	FrameJob5CAllRegsEntry	0,0,switch2hires,0
	;;
	FrameJob5CAllRegsEntry	10,0,smoking_hot_init
	FrameJob5CAllRegsEntry	0,2,smoking_hot_run


	FrameJob5CAllRegsEntry	$FFFF,0,0,0


	.code
_main:
	ldax	#Code_cstack		; C-Stack
	stax	sp
	lda	$d020		; Retrieve border colour.
	jsr	_fill_colour_ram
	;; 	jsr	_main_squiral_fader
	lda	#0
	sta	$d011
	ldax	#_frame0000+2+RNR_width*RNR_height
	jsr	_analyse_colourballs
	PushWordLH	#norocknrollposX
	PushWordLH	#norocknrollposY
	PushWordLH	#_frame0000+2
	ldx	#RNR_width		; Width
	ldy	#RNR_height		; Height
	jsr	init_rocknroll
	jsr	init
	ldax	#eventtable
	ldy	#0
	jsr	frameengine_5c_allregs_init
	cli
	jmp	*
