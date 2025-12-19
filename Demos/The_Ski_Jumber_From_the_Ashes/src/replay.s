	.include	"LAMAlib.inc"
	.include	"resident.i"
	.include	"globals.i"
	.include	"t7d/koala.i"
	.include	"t7d/vic/screen.i"
	.include	"t7d/theatre/consts.i"
	.include	"t7d/sprite/spriteX2E.i"
	.include	"t7d/sprite/spritepad.i"
	.include	"zeropage.inc"
	.include	"moiety.frameengine.i"

;;; Replay the moving down of the ski jump ramp and show the lift-off... Change music to Major Tom?

	JUMPER_SPRITE_ADDR=THEATRE_SPRT
	JUMPER_SPRITE_PTR=(JUMPER_SPRITE_ADDR&$3fff)/64
	JUMPER_X=(24+270)/2
	JUMPER_Y=50+2
	BIGR_ADDR=THEATRE_SPRT+64
	BIGR_SPRITE_PTR=(BIGR_ADDR&$3fff)/64
	BIGR_SPRPAD=23
	BIGR_COL=0

	.rodata
nextpartname:	.asciiz	"earth_to_orbit.Z"
schanze:	.incbin	"assets/skijump-seitlich-nur-hintergrund.kla",2
spritedata:	.incbin	"assets/slow.skijumper.spd"

	.data
finished:	.byte	0	; Finish demo if Bit 7 is set.
spriteworkarea:
	.byte	JUMPER_X, 50/2, 0, 0, 0, 0, 0, 0
	.byte	JUMPER_Y, 50+20, 0, 0, 0, 0, 0, 0
	spriteareaX=spriteworkarea
	spriteareaY=spriteworkarea+8
	spriteareaDX=spriteworkarea+16
	spriteareaDY=spriteworkarea+24
	.res	8*2

;;; Frame#, channel, address, A
;;; Channels 0-1: one-shot (pointers deleted)
;;; Channels 2-3: called every frame
;;; Channel 4: continues running (but pointer deleted)
engine_data:
	JobEntry	50*4,0,set_sprite_dx,$FF
	JobEntry	30,0,set_sprite_dx,0

	JobEntry	25,0,new_sprite,10
	JobEntry	0,1,jump_y_sprite,6

	JobEntry	25,0,set_sprite_dx,$FF
	JobEntry	0,1,set_sprite_dy,1

	JobEntry	10,1,set_sprite_dy,2
	JobEntry	5,1,set_sprite_dy,1
	JobEntry	10,1,set_sprite_dy,2
	JobEntry	5,1,set_sprite_dy,1
	JobEntry	10,1,set_sprite_dy,2
	JobEntry	5,1,set_sprite_dy,1
	JobEntry	10,1,set_sprite_dy,2
	JobEntry	5,1,set_sprite_dy,1
	JobEntry	10,1,set_sprite_dy,2
	JobEntry	5,1,set_sprite_dy,1
	JobEntry	10,1,set_sprite_dy,2
	JobEntry	5,1,set_sprite_dy,1
	JobEntry	10,1,set_sprite_dy,2
	JobEntry	5,1,set_sprite_dy,1
	JobEntry	10,1,set_sprite_dy,2
	JobEntry	5,1,set_sprite_dy,1

	JobEntry	27,4,new_sprite,11
	JobEntry	0,0,jump_y_sprite,6

	JobEntry	1,0,set_sprite_dx,$FF
	JobEntry	0,1,set_sprite_dy,0

	JobEntry	35,0,set_sprite_dx,$FF
	JobEntry	0,1,set_sprite_dy,256-4

	JobEntry	180,0,set_sprite_dy,0
	JobEntry	0,1,set_sprite_dx,0

	JobEntry	30,0,finishpart,0

	JobEntry	$FFFF,0,0,0

	.segment	"ONCE"
	.segment	"EXEHDR"
	.export	__EXEHDR__:absolute=1
	jsr	main
	jmp	nextpart
Start:

	.segment	"LOWCODE"
.proc nextpart
	ldax	#nextpartname
	ldy	#RESIDENT_load8000|RESIDENT_pucrunch|RESIDENT_load_RTS
	jsr	RESIDENT_load_nextpart
	ldax	#datasette
	ldy	#RESIDENT_datasette_default_irq|RESIDENT_datasette_jmp_33c
	jsr	RESIDENT_copy_datasette
	.byte	2
datasette:
	.org	$33c
	lda	$d011
	and	#%01101111
	sta	$d011
	DELAYTIME=167
	delay_ms	DELAYTIME
	lda	#14
	sta	$d020
	delay_ms	DELAYTIME
	lda	#6
	sta	$d020
	delay_ms	DELAYTIME
	lda	#0
	sta	$d020
	jmp	RESIDENT_decrunch_last_loaded
	.reloc
.endproc

.proc	finishpart
	lda	#$FF
	sta	finished
	rts
.endproc

	.code
.proc	set_sprite_dx
	sta	spriteareaDX
	rts
.endproc

.proc	set_sprite_dy
	sta	spriteareaDY
	rts
.endproc

.proc	jump_y_sprite
	clc
	adc	spriteareaY
	sta	spriteareaY
	rts
.endproc

.proc	new_sprite
	tay
	ldax	#spritedata
	stax	ptr1
	ldax	#JUMPER_SPRITE_ADDR
	;; Y used from above.
	jsr	copy_spad_data
	rts
.endproc

	.code
.proc	init_sprites
	ldax	#spritedata
	stax	ptr1
	ldax	#JUMPER_SPRITE_ADDR
	ldy	#11
	jsr	copy_spad_data
	ldax	#BIGR_ADDR
	ldy	#BIGR_SPRPAD
	jsr	copy_spad_data
	;; 
	lda	#JUMPER_SPRITE_PTR
	sta	THEATRE_SPRT_PTR
	lda	#BIGR_SPRITE_PTR
	sta	THEATRE_SPRT_PTR+1
	;; 
	lda	#%00000011
	sta	$d015		; Turn sprites on.
	lda	#%00000001
	sta	$d01c		; Multicolour.
	lda	#%00000010
	sta	$d017
	sta	$d01d
	getSPadColour	spritedata,11
	sta	$d027
	getSPadColour	spritedata,BIGR_SPRPAD
	sta	$d028
	getSPadMCColour1	spritedata
	sta	$d025
	getSPadMCColour2	spritedata
	sta	$d026
	ldax	#spriteworkarea
	ldy	#1
	jsr	sprX2E_init
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

	.code
.proc	irq
	lda	frameengine_frameno
	ldx	#0
	stx	tmp1
	.repeat	3
	asl
	rol	tmp1
	.endrepeat
	lda	tmp1
	and	#%00000010
	sta	tmp1
	lda	$d015
	and	#%11111101
	ora	tmp1
	sta	$d015
	jsr	sprX2E_copy_sprite_shadowregs
	lda	#1
	and	frameengine_frameno
	beq	no_update
	jsr	sprX2E_update_sprites
no_update:
	jsr	frameengine_run
	rts
.endproc

	.code
main:
	sei
	jsr	RESIDENT_set_default_irq
	cli
	jsr	vic_screen_msb_turn_off
	jsr	init_sprites
	jsr	copy_koala_picture_data_bg
	.word	schanze
	.word	THEATRE_GPHX
	.word	THEATRE_TEXT
	.word	$D800
	jsr	setup_gfx
	lda	#1
	sta	$d020
	ldax	#engine_data
	ldy	#0
	jsr	frameengine_init
	sei
	ldax	#irq
	jsr	RESIDENT_set_isr_call
	cli
waitend:
	bit	finished
	bpl	waitend
	rts
