	.include	"t7d/memoryconfig.i"
	.include	"t7d/memoryfunctions.i"
	.include	"t7d/koala.i"
	.include	"t7d/theatre/consts.i"
	.include	"t7d/petsciicopyframe.i"
	.include	"LAMAlib.inc"
	.include	"resident.i"
	.include	"t7d/sprite/spritepad.i"
	.include	"zeropage.inc"
	.include	"globals.i"
	.macpack	cbm

	.import	_frame0000

	.export	PETSCIICOPY_SCREEN=THEATRE_TEXT

	JUMPER_X=45
	.define JUMPER_X_CHAR 9
	JUMPER_Y=0
	JUMPER_SPRITE_ADDR=THEATRE_SPRT
	JUMPER_SPRITE_PTR=(JUMPER_SPRITE_ADDR&$3fff)/64
	JUMPER_SPRITEPAD0=9	; Spritepad ID for sprite 0.
	JUMPER_SPRITEPAD1=8

	SPRITE_BOTTOM=248

	.zeropage
textptr:	.res	2

	.rodata
nextpartname:	.asciiz	"replay.Z"
spritedata:	.incbin	"assets/slow.skijumper.spd"

credits:
	;;       1234567890123456789012345
	scrcode	"    graphics: logiker    "
	scrcode	"   graphics: bala-koala  "
	scrcode	"     code: pararaum      "
	scrcode	"    code, music: wil     "
	scrcode	"  music: nordischsound   "
	scrcode	"   music: praiser, bsc   "
	scrcode	"     font: damieng       "
	.byte	$FF

text_xpos:
	.repeat	25,I
	.byte	JUMPER_X_CHAR+I
	.endrepeat

text_ypos:
	.repeat	25,I
	.byte	I
	.endrepeat

	.data
counter:	.byte	$FF	; Simple frame counter.
effect:	.byte	1		; Current effect number.

	.segment	"ONCE"
	.segment	"EXEHDR"
	.export	__EXEHDR__:absolute=1
	sei
	jsr	init
	cli
	jsr	main
	jmp	nextpart
Start:

	.segment	"LOWCODE"
.proc nextpart
	memoryconfig_kernal
	ldax	#lowcode
	ldy	#RESIDENT_datasette_jmp_33c|RESIDENT_datasette_irq_33f
	jmp	RESIDENT_copy_datasette
lowcode:
	.org	$33C
	.scope
	jmp	lowmain
datasetteirq:
	jsr	blank
	jsr	blank
	jsr	blank
	jsr	blank
	jmp	blank
blank:
	ldax	lfsr_register
	stax	ptr1
	inc16	ptr1,THEATRE_TEXT
	ldy	#0
	lda	#$a0		; Inverse space.
	sta	(ptr1),y
	ldax	lfsr_register
	stax	ptr1
	inc16	ptr1,$D800
	lda	#1		; White
	sta	(ptr1),y
	;; Next lfsr value.
	lsr     lfsr_register+1 ; Shift HI to the right.
        ror     lfsr_register   ; Shift rest (LO) to the right.
        bcc     @not              ; No Term to XOR.
        lda     #<lfsr_feedbackterm
        eor     lfsr_register
        sta     lfsr_register
        lda     #>lfsr_feedbackterm
        eor     lfsr_register+1
        sta     lfsr_register+1
@not:
	rts
lfsr_register:	.word	1	; Current lfsr register value.
	lfsr_feedbackterm=$390	; https://users.ece.cmu.edu/~koopman/lfsr/
lowmain:
	lda	#0
	sta	$d015		; Remove all sprites.
	ldax	#nextpartname
	ldy	#RESIDENT_load8000|RESIDENT_pucrunch|RESIDENT_load_RTS|RESIDENT_keep_irq
	jsr	RESIDENT_load_nextpart
	lda	#$a0		; Clear top-left corner as LFSR can never be zero.
	sta	THEATRE_TEXT
	lda	#1
	sta	$d800
	bit	Control_single_mode ; Are we in standalone mode?
	bmi	*		    ; Do not continue but stop.
	lda	$d011
	and	#%01101111
	sta	$d011
	jmp	RESIDENT_decrunch_last_loaded
	.endscope
	.reloc
.endproc

	.code
.proc	init
	bit	after		; Makes debugging easier.
	CopyFileIntoMemoryDown  $1000,"assets/La_Fusiona_de_Dangera.sid",$7c+2
after:	lda	#0
	jmp	$1000
.endproc

	.code
.proc	next_effect
	inc	effect
	lda	#$FF
	sta	counter
	rts
.endproc

	.code
.proc	move_sprite_down
	lda	#SPRITE_BOTTOM
	cmp	$d001
	beq	out
	inc	$d000
	bne	no_x_overflow1
	lda	$d010
	ora	#3
	sta	$d010
no_x_overflow1:
	inc	$d000
	bne	no_x_overflow2
	lda	$d010
	ora	#1
	sta	$d010
no_x_overflow2:
	inc	$d001
	inc	$d001
	ldax	$d000
	stax	$d002
out:	rts
.endproc

.proc	display_next_char
	lda	$d001
	cmp	#50
	bcc	outside
	sec
	sbc	#50		; Position after the character.
	lsr			; Divide by eight.
	lsr
	lsr
	tax			; Line in X, Y.
	lda	#0
	sta	ptr1+1		; HI of position.
	lda	text_ypos,x
	sta	ptr1		; *8
	asl	ptr1
	rol	ptr1+1
	asl	ptr1
	rol	ptr1+1
	asl	ptr1
	rol	ptr1+1
	lda	ptr1		; ptr2 (16bit)=ptr1
	sta	ptr2
	lda	ptr1+1
	sta	ptr2+1
	asl	ptr1		; *16
	rol	ptr1+1
	asl	ptr1		; *32
	rol	ptr1+1
	clc
	lda	ptr2
	adc	ptr1
	sta	ptr1
	lda	ptr2+1
	adc	ptr1+1
	sta	ptr1+1
	lda	text_xpos,x
	clc
	adc	ptr1
	sta	ptr1
	lda	ptr1+1
	adc	#0
	sta	ptr1+1
	;; Add screen position to ptr1.
	lda	#<PETSCIICOPY_SCREEN
	clc
	adc	ptr1
	sta	ptr1
	lda	#>PETSCIICOPY_SCREEN
	adc	ptr1+1
	sta	ptr1+1
	txa
	tay
	lda	(textptr),y
	ldy	#0
	eor	#$80		; Reverse the char.
	sta	(ptr1),y
outside:
	rts
.endproc

.proc	reset_to_next_line
	inc16	textptr,25
	lda	#JUMPER_Y
	sta	$d001
	lda	#JUMPER_X
	sta	$d000
	ldy	#0
	sty	$d010
	rts
.endproc

	.code
.proc	lift_up_characters
	.repeat	40-JUMPER_X_CHAR,COL
	.repeat	COL,ROW
	.if ROW<25
	lda	PETSCIICOPY_SCREEN+ROW*40+COL+40+JUMPER_X_CHAR
	sta	PETSCIICOPY_SCREEN+ROW*40+COL+JUMPER_X_CHAR
	.endif
	.endrepeat
	lda	#$A0
	.if COL<25
	sta	PETSCIICOPY_SCREEN+COL*40+JUMPER_X_CHAR+COL
	.else
	sta	PETSCIICOPY_SCREEN+24*40+JUMPER_X_CHAR+COL
	.endif
	.endrepeat
	rts
.endproc

	.code
.proc	irq
	inc	counter
	lda	effect
	on_A_jmp	eff_down, wait_100, eff_lift_up, wait_100, next_line
	lda	#1
	sta	effect
out:	rts
eff_down:
	jsr	move_sprite_down
	jsr	display_next_char
	lda	$d001		; Y-Position
	cmp	#SPRITE_BOTTOM
	bne	out
	jmp	next_effect
eff_lift_up:			; Lift up characters
	jsr	lift_up_characters
	lda	counter
	cmp	#50
	bne	out
	jmp	next_effect
wait_100:			; Wait 100 Frames.
	lda	counter
	cmp	#100
	bne	out
	jmp	next_effect
next_line:
	jsr	reset_to_next_line
	jmp	next_effect
.endproc

	.code
main:
	jsr	RESIDENT_set_default_irq
	cli
	ldax	#credits
	stax	textptr
	ldax	#_frame0000
	jsr	petsciicopyframe
	lda	#$1b		; Textscreen.
	sta	$d011
	lda	#<~(1<<4)	; Multicol off.
	lda	#$04		; Font at $d000
	sta	$d018
	lda	#$c8
	sta	$d016
	ldax	#spritedata
	stax	ptr1
	ldax	#JUMPER_SPRITE_ADDR
	ldy	#JUMPER_SPRITEPAD0
	jsr	copy_spad_data
	ldax	#JUMPER_SPRITE_ADDR+64
	ldy	#JUMPER_SPRITEPAD1
	jsr	copy_spad_data
	lda	#JUMPER_SPRITE_PTR
	sta	THEATRE_SPRT_PTR
	;; 	ldx	THEATRE_SPRT_PTR
	tax
	inx
	stx	THEATRE_SPRT_PTR+1
	lda	#3
	sta	$d015		; Enable sprite 0 & 1.
	sta	$d017		; Double height.
	sta	$d01d		; Double width.
	lda	#2
	sta	$d01c		; Multicolour.
	getSPadColour	spritedata,JUMPER_SPRITEPAD0
	sta	$d027
	getSPadColour	spritedata,JUMPER_SPRITEPAD1
	sta	$d028
	getSPadMCColour1	spritedata
	sta	$d025
	;; 	getSPadMCColour2	spritedata
	lda	#2		; Red.
	sta	$d026
	lda	#JUMPER_X
	sta	$d000
	lda	#JUMPER_Y
	sta	$d001
	ldax	#irq
	jsr	RESIDENT_set_isr_call
waitend:
	ldy	#0
	lda	(textptr),y
	bpl	waitend
	jsr	RESIDENT_set_default_irq
	rts
