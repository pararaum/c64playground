;;; asm6502 -l -w -e sprite_text_display.asm -b 2047

SPRX = $fe
SPRITE_DATAAREA = $4000-64*8
SPRITE_POINTER = $7f8

LINEIDX = 2
LINEPTR = 3
DATAPTR = 5
TMPPTR = $FE

	.org $0801 - 2
	.word $0801
	.word	end_of_basic	; 0801
	.word	$91d9		; 0803
	.byte	$9e,$20,"2080 n THE 7TH DIVISI",$91
end_of_basic:
	.byte	0,0,0

	cld
	jmp	main
framecounter:
	.byte	0		; Just for counting frames, it is incremented every frame
	;; The sprite position and velocity is multiplied by 16.
sprite_x_position:
	.word	0
sprite_y_position:
	.word	0
sprite_x_velocity:
	.byte	0
sprite_y_velocity:
	.byte	0
irqroutine:
	lda	$d011
	and	#~%00001000
	sta	$d011
	lda	#$ff
	sta	$d019		; Acknowledge interrrupt
	dec	$d020
	;; 
	inc	framecounter
	jsr	$1003		; Play Muzak
	jsr	display_sprite
	jsr	move_sprite
	;;
	inc	$d020
	lda	$d011
	and	#%01111111
	ora	#%00001000
	sta	$d011
	lda	#$fa
	sta	$d012
	jmp	$ea31

main:	sei
	ldx	#$ff
	txs
	jsr	init_fun
	lda	#0
	jsr	$1000		; Init Muzak.
	cli
.mloop:	nop
	lda	#<text
	sta	LINEPTR
	lda	#>text
	sta	LINEPTR+1
	jsr	set_sprite_position
	.byte	1,1
	.word	$50
	.byte	$37
	jsr	copy_2_lines_to_sprite
	jsr	fade_cycle_sprite
	lda	#67
	jsr	wait_frames
	jsr	set_sprite_position
	.byte	-1,1
	.word	$80
	.byte	$32
	jsr	copy_2_lines_to_sprite
	jsr	fade_cycle_sprite
	lda	#67
	jsr	wait_frames
	jsr	set_sprite_position
	.byte	1,-1
	.word	$30
	.byte	$62
	jsr	copy_2_lines_to_sprite
	jsr	fade_cycle_sprite
	lda	#67
	jsr	wait_frames
	jsr	set_sprite_position
	.byte	-1,-1
	.word	$a9
	.byte	$68
	jsr	copy_2_lines_to_sprite
	jsr	fade_cycle_sprite
	lda	#67
	jsr	wait_frames
	jsr	set_sprite_position
	.byte	0,-1
	.word	$41
	.byte	$41
	jsr	copy_2_lines_to_sprite
	jsr	fade_cycle_sprite
	php
	sec
	clv
	plp
	nop
	jmp	.mloop


display_sprite:
	lda	sprite_y_position+1
	sta	.high
	lda	sprite_y_position
	;; Sprite position is now in .high and A.
	lsr	.high		; Divide by 16
	ror
	lsr	.high
	ror
	lsr	.high
	ror
	lsr	.high
	ror
	tay
	lda	sprite_x_position+1
	sta	.high
	lda	sprite_x_position
	;; Sprite position is now in .high and A.
	lsr	.high		; Divide by 16
	ror
	lsr	.high
	ror
	lsr	.high
	ror
	lsr	.high
	ror
	tax
	lda	.high
	jmp	position_sprite
.high:	.byte	0


move_sprite:
	lda	sprite_x_velocity
	clc
	adc	sprite_x_position
	sta	sprite_x_position
	lda	sprite_x_velocity
	ora	#$7f		; Sign extend, see https://wiki.nesdev.com/w/index.php/Synthetic_instructions
	bmi	.neg1
	lda	#0
.neg1:	adc	sprite_x_position+1
	sta	sprite_x_position+1
	lda	sprite_y_velocity
	clc
	adc	sprite_y_position
	sta	sprite_y_position
	lda	sprite_y_velocity
	ora	#$7f
	bmi	.neg2
	lda	#0
.neg2:	adc	sprite_y_position+1
	sta	sprite_y_position+1
	rts
	

fade_cycle_sprite:
	lda	#50
	jsr	wait_frames
	jsr	fade_in_sprite
	lda	#241
	jsr	wait_frames
	jmp	fade_out_sprite
	

fade_out_sprite:
	jsr	fade_routine_sprite
	.word	.cols
	lda	#$00
	sta	$d015
	rts
.cols:
	.byte	13, 7, 10, 8, 2, 9, 0, -1
	rts

fade_in_sprite:
	lda	#$ff
	sta	$d015
	jsr	fade_routine_sprite
	.word	.cols
	rts
.cols:
	.byte	9, 2, 8, 10, 7, 13, -1

fade_routine_sprite:
	tsx
	lda	$101,x
	sta	TMPPTR
	lda	$102,x
	sta	TMPPTR+1
	ldy	#1
	lda	(TMPPTR),y
	sta	.l78+1
	iny
	lda	(TMPPTR),y
	sta	.l78+2
	ldx	#0
.l78:	lda	$BDBD,x
	bpl	.l74
	tsx			; Correct the return address.
	lda	#2		; One word (two bytes) need to be skipped.
	clc
	adc	$101,x
	sta	$101,x
	lda	#0		; Add carry, if needed.
	adc	$102,x
	sta	$102,x
	rts
.l74:	jsr	colour_sprites
	lda	#4
	jsr	wait_frames
	inx
	bne	.l78		; X is never zero here.
	
;;; A=frames to wait, Destroys: framecounter
wait_frames:
	stx	.old+1		; Save X
	ldx	#0
	stx	framecounter
.l67:	cmp	framecounter
	bne	.l67
.old:	ldx	#0		; Restore X
	;; Trick from https://codebase64.org/doku.php?id=base:introduction_to_raster_irqs
	rts


;;; JSR
;;; vx
;;; vy
;;; x (16 bit)
;;; y
set_sprite_position:
	tsx
	lda	$101,x		; Set temporary pointer to data-1
	sta	TMPPTR
	lda	$102,x
	sta	TMPPTR+1
	lda	#5		; Skip!
	clc
	adc	$101,x
	sta	$101,x
	lda	#0		; Add carry, if needed.
	adc	$102,x
	sta	$102,x
	ldy	#1		; Get data from data after JSR.
	lda	(TMPPTR),y
	sta	sprite_x_velocity
	iny
	lda	(TMPPTR),y
	sta	sprite_y_velocity
	iny
	lda	(TMPPTR),y
	sta	.data
	iny
	lda	(TMPPTR),y
	sta	.data+1
	asl	.data		; *16
	rol	.data+1
	asl	.data		; 
	rol	.data+1
	asl	.data		; 
	rol	.data+1
	asl	.data		; 
	rol	.data+1
	lda	.data
	sta	sprite_x_position
	lda	.data+1
	sta	sprite_x_position+1
	iny
	lda	(TMPPTR),y
	sta	.data
	iny
	lda	(TMPPTR),y
	sta	.data+1
	asl	.data		; *16
	rol	.data+1
	asl	.data		; 
	rol	.data+1
	asl	.data		; 
	rol	.data+1
	asl	.data		; 
	rol	.data+1
	lda	.data
	sta	sprite_y_position
	lda	.data+1
	sta	sprite_y_position+1
	rts			; Return address has been corrected above.
.data:	.word	0

init_fun:
	lda	#0
	sta	$d020
	sta	$d021
	jsr	copy_text_screen
	lda	#6
	jsr	fill_colour_ram
	lda	#0
	jsr	colour_sprites
	lda	#SPRITE_DATAAREA/64
	jsr	set_sprite_pointer
	jsr	set_sprite_position
	.byte	1,1
	.word	$50
	.byte	$32
	lda	#$00
	sta	$d015
	;; lda	#1
	;; sta	sprite_x_velocity
	;; sta	sprite_y_velocity
	;; ldx	#<($50<<3)
	;; stx	sprite_x_position
	;; ldx	#>($50<<3)
	;; stx	sprite_x_position+1
	;; ldy	#<($32<<3)
	;; sty	sprite_y_position
	;; ldy	#>($32<<3)
	;; sty	sprite_y_position+1
	lda	#$7f
        sta	$dc0d               ;disable CIA interrupts
        sta	$dd0d
        lda	$dc0d               ;clear pending interrupts
        lda	$dd0d
	lda	#$fa
	sta	$d012
	lda	$d011
	and	#%01111111
	sta	$d011
	lda	#<irqroutine
	sta	$314
	lda	#>irqroutine
	sta	$315
	lda	#%00000001
	sta	$d01a		; Enable raster interrupt.
	rts

	INCLUDE	"sprite_functions.asm"

copy_text_screen:
	ldx	#0
.l1:	lda	default_screen_data,x
	sta	$0400,x
	lda	default_screen_data+$0100,x
	sta	$0400+$0100,x
	lda	default_screen_data+$0200,x
	sta	$0400+$0200,x
	lda	default_screen_data+$0300,x
	sta	$0400+$0300,x
	dex
	bne	.l1
	rts

fill_colour_ram:
	ldx	#0
.l1:	sta	$d800,x
	sta	$d900,x
	sta	$da00,x
	sta	$db00,x
	inx
	bne	.l1
	rts

END_OF_CODE:

	.org $1000-$7e
muzak_raw:	INCBIN	"Keeper.sid"

CHARSET_RAW:	INCBIN	"beyond_the_ice_palace.64c"
CHARSET = CHARSET_RAW + 2
default_screen_data:
	INCLUDE	"wanderer_above_sea_extended.inc"
text:
	include	"story_text.inc"
	
EOF:
