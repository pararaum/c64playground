	.include	"LAMAlib.inc"
	.include	"t7d/libt7d.i"
	.include	"t7d/pseudo/yielding.i"
	.include	"t7d/sprite/spritepad.i"
	.include	"t7d/sprite/spriteX2E.i"
	.include	"t7d/vic/vicmacros.i"
	.include	"zeropage.inc"
	.include	"globals.i"

	.import	SFXBASE,SFXPLAY

	.export	init_fly_free

	.bss
nextirq:	.res	2

	.rodata
spritepadnumbers:	.byte	0,1,2,8,9,10,17,17


	.code

	.macro NextText text,column,delay
	textcount	.set	textcount+1
	.ident	(.sprintf("vscrolltext%d", textcount)):
	str_enc0	text
	.ident	(.sprintf("COLUMN%d", textcount))=column
	.ident	(.sprintf("DELAY%d", textcount))=delay
	.ident	(.sprintf("REPEAT%d", textcount))=0
	.endmacro

.scope topdownscroller
	textcount	.set	0
	.include "greetings.inc"
	.include "m_topdownscroller.s"
.endscope


	;; 8*X,8*Y-positions of eight sprites.
	.data
spritepositions:
sprite_Xpositions:
	.byte	57,57+24,57+48
	.byte	57,57+24,57+48
	.byte	57+24,0
sprite_Ypositions:
	.byte	125,125,125
	.byte	125+21,125+21,125+21
	.byte	125+42,0
	.res	16		; Space for velocities.

	.data
spritedata:
	.incbin	"jumper.spd"

;;; Set the sprite delta X
.proc	sprdeltax
	ldx	#8-1
l1:	sta	spritepositions+16,x
	dex
	bpl	l1
	rts
.endproc


	.code
irq:	ldax	#actual_irq
	stax	$314
	SetupYielding	yieldingfun,0,0
	lda	#1
	jsr	SFXBASE
actual_irq:
	asl	$d019
	jsr	call_yielding_function
	jsr	yielding_end_reached
	beq	abrupt_end
	jsr	sprX2E_copy_sprite_shadowregs
        m_run	topdownscroller
	bcs	abrupt_end
	jsr	SFXPLAY
	jmp	$ea81
abrupt_end:
	lda	#0
	sta	$d015
	ldax	nextirq
	stax	$314
	jsr	SFXPLAY
	SetChargenAddress	$1000 ; Default font.
	jmp	$ea81
	

	.macro	SpriteMoveX speed,num
	.local	l2
	lda	#<(speed)
	jsr	sprdeltax
	ldy	#num
l2:	jsr	sprX2E_update_sprites
	jsr	call_yield
	dey
	bne	l2
	.endmacro

	.macro	SpriteWait num
	.local	l3
	ldx	#num
l3:	jsr	call_yield
	dex
	bne	l3
	.endmacro


.proc	yieldingfun
	lda	#$7f
	sta	$d015
	sta	$d01c
	sta	$d01d
	lda	#1
	sta	$d021
	getSPadMCColour1	spritedata
	sta	$d025
	getSPadMCColour2	spritedata
	sta	$d026
	.repeat	8,I
	 getSPadColour	spritedata,I
	 sta	$d027+I
	 ldx	#I
	 ldy	spritepadnumbers,x
	 ldax	#spritedata
	 stax	ptr1
	 ldax	#SPRITEAREA+I*$40
	 jsr	copy_spad_data
	 lda	#<((SPRITEAREA+I*$40)/$40)
	 sta	__SCREEN_START__+1024-8+I
	.endrepeat
	ldax	#spritepositions
	ldy	#1
	jsr	sprX2E_init
	jsr	call_yield
	ldx	#0
	lda	#' '
l1:	.repeat	4,I
	 sta	__SCREEN_START__+I*250,x
	.endrepeat
	inx
	cpx	#250
	bne	l1
	SetChargenAddress	ROTATED_FONT
fullloop:	
	jsr	call_yield
	;next Sprite X coordinate
	jsr	get_next_byte
	sta 	sprite_Xpositions
	sta 	sprite_Xpositions+3
	clc
	adc	#24
	sta 	sprite_Xpositions+1
	sta 	sprite_Xpositions+4
	sta 	sprite_Xpositions+6
	;clc
	adc	#24
	sta 	sprite_Xpositions+2
	sta 	sprite_Xpositions+5
	;next Sprite Y coordinate	
	jsr	get_next_byte
	sta 	sprite_Ypositions
	sta 	sprite_Ypositions+1
	sta 	sprite_Ypositions+2
	;clc
	adc	#21
	sta 	sprite_Ypositions+3
	sta 	sprite_Ypositions+4
	sta 	sprite_Ypositions+5
	;clc
	adc	#21
	sta 	sprite_Ypositions+6
	jmp	fullloop
.endproc

	.code
init_fly_free:
	ldax	$314
	stax	nextirq
	SetIRQ314Pointer	irq
        m_init	topdownscroller
	rts

get_next_byte:
coord_data_ptr=*+1
	lda coord_data
	cmp #$ff
	if eq
	  pokew coord_data_ptr, coord_data
          jmp get_next_byte
	endif
	inc16 coord_data_ptr
	rts

coord_data: .incbin "coord_data_440steps.bin"
