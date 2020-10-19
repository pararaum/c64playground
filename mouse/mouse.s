
	.include	"basic.i"
	.export	_main

	SPRITEDATA = $340
	SPRITEX = $d000
	SPRITEY = $d001
	SPRITEBIT = %00000001

	.rodata
welcome:
	.byte	"MOUSE INTERACTION",10,0

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
	lda	#$e7
	sta	SPRITEDATA	; TODO Sprite
	lda	#SPRITEDATA/$40
	sta	2040		; Sprite pointer.
	rts

irq:
	jmp	$ea31

_main:
	jsr	setup_irq
	jsr	setup_sprite
	lda	#<welcome
	ldy	#>welcome
	jsr	STROUT
	rts
