	.include	"t7d/sprite/spritescroller-rol.i"
	.include	"t7d/petscii_screencode.i"
	.include	"LAMAlib-macros16.inc"

	.bss
spritescroller_rol_begtextptr:	.res	2 ; Pointer to the beginning of the scroll text.
SPRITESCROLLER_ROL_Y_POS:	.res	1 ; Y-position of the scroller.

	.zeropage
;;; Pointer to the next character for the scroller.
textptr:	.res	2

	.data
SPRITESCROLLER_COLOUR:
	.byte	13		; Light green

	.define sprite_x_positions 24,24+1*48,24+2*48,24+3*48,24+4*48,24+5*48,24+6*48,24+7*48

sprite_x_positions_lo:
	.lobytes	sprite_x_positions
sprite_x_positions_hi:
	.byte	%11100000	; TODO: Calculate!
;.posiy  .byte (24 < 256)<<0,(24+1*48 < 256)<<1,(24+2*48 < 256)<<2,(24+3*48 < 256)<<3,(24+4*48 < 256)<<4,(24+5*48 < 256)<<5,(24+6*48 < 256)<<6,(24+7*48 < 256)<<7
	.bss
sprite_pointers:	.res	8

	.bss
;;; Buffer for eight bytes to hold the current character.
current_char:
	.res	8
;;; Counter of updates before the next character is drawn.
scroll_counter:
	.res	1

	.code
.proc	reset_text_pointer
	lda	spritescroller_rol_begtextptr
	sta	textptr
	lda	spritescroller_rol_begtextptr+1
	sta	textptr+1
	rts
.endproc

	.code
spritescroller_rol_init:
	stax	spritescroller_rol_begtextptr ; Store the beginning of the scroll text.
	sty	SPRITESCROLLER_ROL_Y_POS
	lda	#0
	sta	scroll_counter	; Reset the scroll counter.
	;; Clear the current character.
	.repeat 8,I
	 sta	current_char+I
	.endrepeat
	tax			; Zero X register.
	;; Clear 512 bytes, the eight sprite buffers.
@l1:	sta	SPRITESCROLLER_ROL_SPRITEBUFFER,x
	sta	SPRITESCROLLER_ROL_SPRITEBUFFER+$100,x
	inx
	bne	@l1
	jmp	reset_text_pointer

	.code
spritescroller_rol_init_sprite_registers:
	lda #$ff		;Sprites on
	sta $d015
	sta $d01d		;double width
	ldy #0
	ldx #0
@l1:	lda sprite_x_positions_lo,x		;X
	sta $d000,y
	lda SPRITESCROLLER_ROL_Y_POS		;Y
	sta $d001,y
	lda SPRITESCROLLER_COLOUR		; Set the colour of all sprites.
	sta $d027,x
	inx
	iny
	iny
	cpx #8
	bne @l1
	lda sprite_x_positions_hi
	sta $d010
	rts

scroll_left:
	ldy	#0
	ldx	#0
@l1:	txa
	pha
	tya
	tax
	asl	current_char,x
	pla
	tax
	rol SPRITESCROLLER_ROL_SPRITEBUFFER+$1c2,x		;7
	rol SPRITESCROLLER_ROL_SPRITEBUFFER+$1c1,x
	rol SPRITESCROLLER_ROL_SPRITEBUFFER+$1c0,x
	rol SPRITESCROLLER_ROL_SPRITEBUFFER+$182,x		;6
	rol SPRITESCROLLER_ROL_SPRITEBUFFER+$181,x
	rol SPRITESCROLLER_ROL_SPRITEBUFFER+$180,x
	rol SPRITESCROLLER_ROL_SPRITEBUFFER+$142,x		;5
	rol SPRITESCROLLER_ROL_SPRITEBUFFER+$141,x
	rol SPRITESCROLLER_ROL_SPRITEBUFFER+$140,x
	rol SPRITESCROLLER_ROL_SPRITEBUFFER+$102,x		;4
	rol SPRITESCROLLER_ROL_SPRITEBUFFER+$101,x
	rol SPRITESCROLLER_ROL_SPRITEBUFFER+$100,x
	rol SPRITESCROLLER_ROL_SPRITEBUFFER+$0c2,x		;3
	rol SPRITESCROLLER_ROL_SPRITEBUFFER+$0c1,x
	rol SPRITESCROLLER_ROL_SPRITEBUFFER+$0c0,x
	rol SPRITESCROLLER_ROL_SPRITEBUFFER+$082,x		;2
	rol SPRITESCROLLER_ROL_SPRITEBUFFER+$081,x
	rol SPRITESCROLLER_ROL_SPRITEBUFFER+$080,x
	rol SPRITESCROLLER_ROL_SPRITEBUFFER+$042,x		;1
	rol SPRITESCROLLER_ROL_SPRITEBUFFER+$041,x
	rol SPRITESCROLLER_ROL_SPRITEBUFFER+$040,x
	rol SPRITESCROLLER_ROL_SPRITEBUFFER+$002,x		;0
	rol SPRITESCROLLER_ROL_SPRITEBUFFER+$001,x
	rol SPRITESCROLLER_ROL_SPRITEBUFFER+$000,x
	txa
	clc
	adc	#3
	tax
	iny
	cpy	#8
	bne	@l1
	rts

;;; Modifies: A, X, Y
next_char:
	ldy	#0
	sty	@GETFONTPTR+1	; Clear HI.
	lda	(textptr),y	; Get next char.
	tax
	lda	PETSCII2SCREENCODES_TABLE,x
	asl
	rol	@GETFONTPTR+1
	asl
	rol	@GETFONTPTR+1
	asl
	rol	@GETFONTPTR+1
	clc
	adc	#<SPRITESCROLLER_ROL_CHARGEN
	sta	@GETFONTPTR
	lda	@GETFONTPTR+1
	adc	#>SPRITESCROLLER_ROL_CHARGEN
	sta	@GETFONTPTR+1
	ldy	#7
@l1:	lda	$ffff,y
	@GETFONTPTR=*-2
	sta	current_char,y
	dey
	bpl	@l1
	;; Increment pointer.
	inc	textptr
	bne	@noovl
	inc	textptr+1
@noovl:
	ldy	#0
	lda	(textptr),y
	bne	@charok
	jsr	reset_text_pointer
@charok:
	rts

spritescroller_rol_update:
	jsr	scroll_left
	dec	scroll_counter	; Decrement each update.
	bpl	@nochar
	lda	#8
	sta	scroll_counter
	jsr	next_char
@nochar:
	rts
