
	.include	"basic.i"
	.import	mouse
	.import mouse_init

	.export	_main
	.export	SPRITEX
	.export	SPRITEY
	.export SPRITEBIT

	SPRITEDATA = $340
	SPRITEX := $d000
	SPRITEY := $d001
	SPRITEBIT = %00000001

	.rodata
welcome:
	.byte	"MOUSE INTERACTION",10,0

butterfly:			; Butterfly sprite from Sally, Sprite Graphics for the Commodore C64, Micro Text Publications, p. 9, 1983.
	;; REM BUTTERFLY SPRITE EXAMPLE
	.byte 2,0,64
	.byte 49,0,140
	.byte 120,129,30
	.byte 252,66,63
	.byte 254,36,127
	.byte 255,24,255
	.byte 255,153,255
	.byte 255,219,255
	.byte 255,255,255
	.byte 255,255,255
	.byte 255,255,255
	.byte 255,255,255
	.byte 255,255,255
	.byte 255,255,255
	.byte 255,255,255
	.byte 255,255,255
	.byte 255,219,255
	.byte 127,153,254
	.byte 63,24,252
	.byte 30,24,120
	.byte 12,24,48


	.code
setup_irq:
	lda	#<irq
	sta	$314
	lda	#>irq
	sta	$315
	rts

setup_sprite:
	;; Top left corner, see S.G. Larsen, Sprite Graphics for the Commodore C64, Micro Text Publications, 1983.
	lda	#24		; X-pos
	sta	SPRITEX
	lda	#50		; Y-pos
	sta	SPRITEY
	lda	#0		; X-msb
	sta	$d010
	lda	#1		; white
	sta	$d027
	lda	#SPRITEBIT
	ora	$d015
	sta	$d015		; Enable sprites
	lda	#SPRITEDATA/$40
	sta	2040		; Sprite pointer.
	;; Copy sprite data.
	ldx	#63
@l1:	lda	butterfly,x
	sta	SPRITEDATA,x
	dex
	bpl	@l1
	rts

irq:	jsr	mouse
	lda	$d419
	sta	$0400
	lda	$d41a
	sta	$0401
	jmp	$ea31

_main:
	jsr	mouse_init
	jsr	setup_irq
	jsr	setup_sprite
	lda	#<welcome
	ldy	#>welcome
	jsr	STROUT
	rts
