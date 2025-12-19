	.include	"t7d/memoryconfig.i"
	.include	"t7d/memoryfunctions.i"
	.include	"t7d/stringfunctions.i"
	.include	"t7d/koala.i"
	.include	"t7d/theatre/consts.i"
	.include	"t7d/stackmacros.i"
	.include	"t7d/libt7d.i"
	.include	"t7d/sprite/sprite.i"
	.include	"t7d/sprite/spritepad.i"
	.include	"t7d/sprite/spritescroller-rol.i"
	.include	"t7d/vic/screen.i"
	.include	"LAMAlib.inc"
	.include	"resident.i"
	.include	"globals.i"
	.include	"zeropage.inc"
	.include	"moiety.frameengine.i"

WAITFRAMES=251
CHARGENADDR=$D800
SCROLLER_Y_POSITION=60
DISTANCE_LIGHTBULB_JUMPER=30
DISTANCEX_LIGHTBULB_JUMPER=12
SNAIL_DELTAX=18

	JUMPER_X=161
	JUMPER_Y=50-25
	JUMPER_SPRNUM1 = 0
	JUMPER_SPRNUM2 = 1
	JUMPER_SPRPAD1 = 0
	JUMPER_SPRPAD2 = 2
	JUMPER_OVRLSPRPAD1 = 1
	JUMPER_OVRLSPRPAD2 = 3
	SNAIL_SPRNUM = 2
	SNAIL_SPRPAD1 = 17
	SNAIL_SPRPAD2 = 18
	QUESTIONMARK_SPRNUM = 3
	QUESTIONMARK_SPRPAD = 14
	LIGHTBULB_SPRPAD = 13
	JET_SPRPAD1 = 15
	JET_SPRPAD2 = 16
	JET_SPRNUM1 = 6
	JET_SPRNUM2 = 7
	JUMPER_SPRITE_ADDR=THEATRE_SPRT
	JUMPER_SPRITE_PTR=(JUMPER_SPRITE_ADDR&$3fff)/64
	SNAIL_SPRITE_ADDR=THEATRE_SPRT+2*64
	SNAIL_SPRITE_PTR=(SNAIL_SPRITE_ADDR&$3fff)/64
	SIGN_SPRITE_ADDR=THEATRE_SPRT+3*64
	SIGN_SPRITE_PTR=(SIGN_SPRITE_ADDR&$3fff)/64

	.export	SPRITESCROLLER_ROL_SPRITEBUFFER=THEATRE_SPRT
	.export	SPRITESCROLLER_ROL_CHARGEN=CHARGENADDR

	.rodata
nextpartname:	.asciiz	"the-jump.Z"
schanze:	.incbin	"assets/skijump.kla",2
schanzeoben:	.incbin	"assets/schanze-von-oben.kla",2
springer:	.incbin	"assets/springer-von-vorne.kla",2
font:		.incbin	"Orbiter.both.64c.zx02"
sfxdata:	.incbin	"sfx.1000.prg.zx02"
farawaytext:	.incbin	"assets/on_a_mountain_range.bin"
spritedata:	.incbin	"assets/slow.skijumper.spd"
spritenumbers:
	.byte	JUMPER_SPRPAD1
	.byte	JUMPER_OVRLSPRPAD1
	.byte	SNAIL_SPRPAD1
	.byte	QUESTIONMARK_SPRPAD
	.byte	0
	.byte	0
	.byte	JET_SPRPAD1
	.byte	JET_SPRPAD2

;;; Frame#, channel, address
;;; Channels 0-1: one-shot (pointers deleted)
;;; Channels 2-3: called every frame
;;; Channel 4: continues running (but pointer deleted)
engine_data:
	JobEntry	0,4,schanze1,0
	JobEntry	400,4,display_text,0
	JobEntry	400,4,springer1,0
	JobEntry	0,0,enable_scroller,0

	JobEntry	117*8,0,disable_scroller,0
	JobEntry	200+30,4,schanze2,0
	JobEntry	290,4,switch2sfx,1  ; Initialise with Song number 1.
	JobEntry	10,0,init_sprites,0 ; 10 Frames for copying, etc.
	JobEntry	0,2,move_down,0
	JobEntry	0,1,initsong,0

	JobEntry	100,2,0,0
	JobEntry	100,2,move_down,0
	JobEntry	0,1,initsong,0

	JobEntry	100,2,0,0
	JobEntry	100,2,move_down,0
	JobEntry	0,1,initsong,0

	JobEntry	100,2,0,0
	JobEntry	50,3,move_snail_down,0
	JobEntry	0,0,initsong,2

	JobEntry	50,0,question_mark,1
	JobEntry	175,0,initsong,1
	JobEntry	0,3,0,0

	JobEntry	125,0,exclamation_mark,1
	JobEntry	0,1,initsong,3

	JobEntry	300,0,exclamation_mark,0
	JobEntry	100,0,initsong,4
	JobEntry	0,2,move_down_fast,0
	JobEntry	0,3,jet_mark,0

	JobEntry	75,2,0,0
	JobEntry	0,3,0,0

	JobEntry	220,0,finishpart,0
	JobEntry	$FFFF,0,0,0
	

	.bss
storage_msb_bits:	.res	1 ; Store the $d010 register for the vanishing sprite effect.
	;; Maybe later...: decrunch_area:	.res	11000

	.data
finished:	.byte	0	; Finish demo if Bit 7 is set.
scroller_on:	.byte	$0	; Call scroller update if Bit 7 is set.

	.segment	"ONCE"
	.segment	"EXEHDR"
	.export	__EXEHDR__:absolute=1
	jsr	main
	jmp	nextpart
Start:

	.segment	"LOWCODE"
.proc nextpart
	jsr	RESIDENT_set_default_irq
	ldax	#cassettebuffer
	ldy	#RESIDENT_datasette_irq_33f
	jsr	RESIDENT_copy_datasette
	ldax	#nextpartname
	ldy	#RESIDENT_load8000|RESIDENT_pucrunch|RESIDENT_keep_irq
	jmp	RESIDENT_load_nextpart
.endproc


cassettebuffer:
	.scope
	.org	$33c
	jmp	64738
	GFXPTR=ptr1
	GFXPTR2=ptr2
	;; ISR
	ldx	linetop
	inc	linetop
	inc	linetop
	lda	hiresE000lo,x
	sta	GFXPTR
	lda	hiresE000hi,x
	sta	GFXPTR+1
	jsr	delete_line
	ldx	linebot
	dec	linebot
	dec	linebot
	lda	hiresE000lo,x
	sta	GFXPTR
	lda	hiresE000hi,x
	sta	GFXPTR+1
	jsr	delete_line
	lda	#$FF
	cmp	linebot		; Have we wrapped?
	bne	out
	ldax	#out
	jmp	RESIDENT_set_isr_call
out:	rts
	;;
	.proc	delete_line	; Input/Modifies: GFXPTR,Y
	ldy	#0
l1:	lda	#0
	sta	(GFXPTR),y
	tya
	clc
	adc	#8
	tay
	bne	l1
	inc	GFXPTR+1
l2:	lda	#0
	sta	(GFXPTR),y
	tya
	clc
	adc	#8
	tay
	cpy	#8*8
	bne	l2
	rts
	.endproc
	;; For calculation see F. Riemenschneider, C64/C128 alles über Maschinensprache, Markt&Technik Verlag, 1988, p. 208.
	;; Or see M. L. De Jong, Assembly Language Programming with the Commodore 64, Brady, 1984, p. 212.
hiresE000hi:	.repeat	200,YY
	.byte	>(THEATRE_GPHX+40*(YY&248)+YY&7)
	.endrepeat
hiresE000lo:	.repeat	200,YY
	.byte	<(THEATRE_GPHX+40*(YY&248)+YY&7)
	.endrepeat
linetop:	.byte	0
linebot:	.byte	199
	.out	.sprintf("Cassete end=$%04X",*)
	.reloc
	.endscope
	.assert *-cassettebuffer<1024,error,"Cassette overflow!"

	.code
.proc	switch2sfx
	pha			; Store song number on stack.
	sei			; No interrupts, be quick!
	PushWordLH	#$1000-2	; Destination, two bytes early because of start address.
	ldax	#sfxdata
	jsr	RESIDENT_decrunch_zx02
	pla
	jsr	$1000
	cli
	rts
.endproc


	.code
.proc	enable_scroller
	lda	#$ff
	sta	scroller_on
	rts
.endproc

;;; Disabe scroller, also turn sprites off.
.proc	disable_scroller
	lda	#$0
	tax			; X=0
	sta	scroller_on
	sta	$d015
	lda	#120		; Move all sprites to the right frame.
l1:	sta	$d000,x
	inx
	inx
	cpx	#16
	bne	l1
	lda	#$ff
	sta	$d010		; MSBs on, so position (+ 90 256).
	ldax	#irq_bottom	; Our own interrupt service routine.
	stax	$FFFE
	rts
.endproc


	.code
.proc	setup_gfx
	lda	#$3b
	sta	$d011
	lda	$d016
	ora	#$10
	sta	$d016
	lda	#%00001000
	sta	$d018
	rts
.endproc

.proc	init_sprites
	PushWordHL	#THEATRE_SPRT
	ldax	#spritedata
	jsr	spritepad_initialise_spritepad
	jsr	spritepad_setup_vic
	ldax	#spritenumbers
	jsr	spritepad_copy8_spritedata
	;; 
	lda	#JUMPER_X
	sta	$d000
	sta	$d002
	lda	#JUMPER_Y
	sta	$d001
	sta	$d003
	lda	#%00001111
	sta	$d015
	lda	#%00000011	; Normal width and height except ski jumper.
	sta	$d017
	sta	$d01d
	lda	#0		; MSBs off.
	sta	$d010
	rts
.endproc


	.code
main:
	sei
	jsr	RESIDENT_set_default_irq
	cli
	lda	#%01101111
	and	$d011
	sta	$d011
	ldax	#scrollertext
	ldy	#SCROLLER_Y_POSITION
	jsr	spritescroller_rol_init
	lda	#0
	sta	SPRITESCROLLER_COLOUR
	jsr	spritescroller_rol_init_sprite_registers
	SetConsecutiveSpriteBufs	THEATRE_SPRT, THEATRE_SPRT_PTR
	lda	#1
	sta	$d020
	sta	$d021
	memoryconfig_ram
	PushWordLH	#CHARGENADDR-2	; Destination, two bytes early because of start address
	ldax	#font
	jsr	RESIDENT_decrunch_zx02
	ldax	#engine_data
	ldy	#0
	jsr	frameengine_init
	ldax	#irq
	jsr	RESIDENT_set_isr_call
waitend:
	bit	finished
	bpl	waitend
	rts

.proc	wait_frames
	dec	counter
	bne	skip
	lda	#WAITFRAMES
	sta	counter
skip:	rts
	.pushseg
	.data
counter:	.byte	WAITFRAMES
	.popseg
.endproc

.proc	display_text
	jsr	vic_screen_msb_turn_off
	Memcpy1000Inline	farawaytext,MEMMAP_gfxarea
	lda	$d016
	and	#<~(1<<4)
	sta	$d016
	lda	#7
	sta	$d018
	lda	#1
	sta	$d020
	sta	$d021
	cli
	lda	#6
	jsr	_fill_colour_ram
	lda	#$1b
	sta	$d011
	rts
.endproc

.proc	springer1
	lda	#%01101111
	and	$d011
	sta	$d011
	jsr	copy_koala_picture_data_bg
	.word	springer
	.word	THEATRE_GPHX
	.word	THEATRE_TEXT
	.word	CHARGENADDR
	jmp	setup_gfx
.endproc

.proc	schanze1
	lda	#%01101111
	and	$d011
	sta	$d011
	jsr	copy_koala_picture_data_bg
	.word	schanze
	.word	THEATRE_GPHX
	.word	THEATRE_TEXT
	.word	CHARGENADDR
	jmp	setup_gfx
.endproc

.proc	schanze2
	lda	#%01101111
	and	$d011
	sta	$d011
	cli
	jsr	copy_koala_picture_data_bg
	.word	schanzeoben
	.word	THEATRE_GPHX
	.word	THEATRE_TEXT
	.word	CHARGENADDR
	jmp	setup_gfx
.endproc

.proc	finishpart
	lda	#$FF
	sta	finished
	lda	#0
	sta	$d015
	rts
.endproc


	.code
.proc	animate_jumper
	lda	flipflop
	eor	#$FF
	sta	flipflop
	beq	spriteflagZ
	copySPadData	spritedata, JUMPER_SPRPAD2, THEATRE_SPRT
	copySPadData	spritedata, JUMPER_OVRLSPRPAD2, THEATRE_SPRT+1*64
	rts
spriteflagZ:
	copySPadData	spritedata, JUMPER_SPRPAD1, THEATRE_SPRT
	copySPadData	spritedata, JUMPER_OVRLSPRPAD1, THEATRE_SPRT+1*64
	rts
	.pushseg
	.data
flipflop:	.byte	0
	.popseg
.endproc

.proc	move_down
	do_every 3
	 inc	$d001
	 inc	$d003
	end_every
	do_every 9
	 jsr	animate_jumper
	end_every
	rts
.endproc

.proc	move_down_fast
	.repeat	2
	 inc	$d001
	 inc	$d003
	.endrepeat
	lda	$d015
	and	#%00000011	; First two sprites turned of?
	beq	no_animation
	do_every 5
	 jsr	animate_jumper
	end_every
	lda	$d001		; Jumper Y into A.
	cmp	#250+(JUMPER_Y&1)
	bcc	no_remove_jumper
	lda	$d015
	and	#<~((1<<JUMPER_SPRNUM1)|(1<<JUMPER_SPRNUM2))
	sta	$d015
no_animation:
no_remove_jumper:
	rts
.endproc


	.code
.proc	move_snail_down
	lda	#JUMPER_X-SNAIL_DELTAX
	sta	$d000+2*SNAIL_SPRNUM
	inc	$d001+2*SNAIL_SPRNUM
	ldax	#spritedata
	stax	ptr1
	do_every 7
	ldax	#SNAIL_SPRITE_ADDR
	ldy	spritepadnum
	jsr	copy_spad_data
	lda	spritepadnum
	eor	#SNAIL_SPRPAD1^SNAIL_SPRPAD2
	sta	spritepadnum
	end_every
	rts
	.data
spritepadnum:	.byte	SNAIL_SPRPAD1
.endproc

	.code
.proc	question_mark
	lda	$d001		; Jumper Y
	sbc	#DISTANCE_LIGHTBULB_JUMPER
	sta	$d001+2*QUESTIONMARK_SPRNUM
	lda	$d000		; Jumper X
	clc
	adc	#DISTANCEX_LIGHTBULB_JUMPER
	sta	$d000+2*QUESTIONMARK_SPRNUM
	rts
.endproc

.proc	exclamation_mark
	cmp	#0		; A is zero?
	beq	off
	lda	$d001		; Jumper Y
	sbc	#DISTANCE_LIGHTBULB_JUMPER
	sta	$d007
	lda	$d000		; Jumper X
	clc
	adc	#DISTANCEX_LIGHTBULB_JUMPER
	sta	$d006
	ldax	#spritedata
	stax	ptr1
	ldax	#SIGN_SPRITE_ADDR
	ldy	#LIGHTBULB_SPRPAD
	jsr	copy_spad_data
	getSPadColour	spritedata,LIGHTBULB_SPRPAD
	sta	$d02a
	lda	#SIGN_SPRITE_PTR
	sta	THEATRE_SPRT_PTR+3
	rts
off:	lda	#<~(1<<3)
	and	$d015
	sta	$d015
	rts
.endproc

.proc	jet_mark
	lda	$d001		; Jumper Y
	sbc	#20
	sta	$d001+2*JET_SPRNUM1
	sta	$d001+2*JET_SPRNUM2
	do_once
	 lda	$d000		; Jumper X
	 clc
	 adc	#DISTANCEX_LIGHTBULB_JUMPER
	 sta	$d000+2*JET_SPRNUM1
	 sta	$d000+2*JET_SPRNUM2
	end_once
	do_once
	 lda	$d015
	 ora	#(1<<JET_SPRNUM1)
	 sta	$d015
	end_once
	do_every	4
	 lda	$d015
	 EOR	#(1<<JET_SPRNUM1)|(1<<JET_SPRNUM2)
	 sta	$d015
	end_every
	rts
.endproc

	.code
.proc	initsong
	;; The LDA #<songnum> is done via the engine.
	jmp	$1000
.endproc


;;; Resident will jsr to here.
.proc	irq
	memoryconfig_io
	jsr	frameengine_run
	bit	scroller_on
	bpl	noscroll
	memoryconfig_charrom
	jsr	spritescroller_rol_update
	memoryconfig_io
noscroll:
	rts
.endproc


.proc	irq_top
	PushRegs
	memoryconfig_io
	asl	$d019
	lda	#80
	sta	$d012
	ldax	#irq_schanze_end
	stax	$FFFE
	lda	$d010
	sta	storage_msb_bits
	lda	#$ff
	sta	$d010
	PullRegs
	rti
.endproc


.proc	irq_schanze_end
	PushRegs
	lda	storage_msb_bits
	sta	$d010
	asl	$d019
	ldax	#irq_bottom
	stax	$FFFE
	lda	#250
	sta	$d012
	PullRegs
	rti
.endproc


.proc	irq_bottom
	PushRegs
	memoryconfig_io
	asl	$d019
	jsr	frameengine_run
	bit	scroller_on
	bpl	noscroll
	memoryconfig_charrom
	jsr	spritescroller_rol_update
	memoryconfig_io
noscroll:
	lda	#40
	sta	$d012
	ldax	#irq_top
	stax	$FFFE
	jsr	$1003
	PullRegs
	rti
.endproc


	.rodata
	;; 	.PUSHCHARMAP
	.repeat	26,I
	.charmap	$41+I,$61+I
	.endrepeat
scrollertext:	.asciiz	"I talked to the Norwegian guys and got their special suit... What could go wrong?                     "
	;; 	.popcharmap
