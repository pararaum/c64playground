; -*- mode: asm -*-

;;; Routine to set the colours of all sprites to the same value.
;;; Input: A=colour
;;; Output: -
;;; Modifies: -
	.import	set_all_sprites_colour


;;; Sprite high bits table for easy and fast access to the high bit of a sprite.
	.import SPRITE_HIGH_BITS

.macro positionSpriteAbsolute num, xpos, ypos
	lda	#<(xpos)
	sta	$d000+num*2
	lda	#(ypos)
	sta	$d001+num*2
	.if	(xpos)>=256
	lda	$d010
	ora	#1<<num
	sta	$d010
	.else
	lda	SPRITE_HIGH_BITS+num
	and	#~(1<<num)
	sta	$d010
	.endif
.endmacro
