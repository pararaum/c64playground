	.include	"t7d/kernal.i"
	.import	TAKEOVER_SPRITEBUFFER
	.import	TAKEOVER_SCREENBASE
	.importzp	ptr1
	.importzp	tmp1

	.export takeover_curtain_left2righ
	.export init_takeover_curtain_left2right
	.export update_takeover_curtain_left2right

TAKEOVER_DISSOLVE_SPEED=0
TAKEOVER_FIRST_SPRITEX=$10	; Just outside the left border.

	.bss
skip_frames:	.res	1
linecounter:	.res	1	; Counter for the lines to process.
topleftscreen:	.res	2	; Pointer to the top left screen corner.
takeover_curtain_left2righ:
	.res	1	; Flag if effect is acting (0=finished).

	.code
.proc	init_takeover_curtain_left2right
	;; Position sprites.
	lda	#50		; Just inside the border
	sta	tmp1
	ldx	#0
l2:	lda	#<((TAKEOVER_SPRITEBUFFER&$3FFF)/$40)
	sta	TAKEOVER_SCREENBASE+1024-8,x ; Sprite pointer
	lda	$d020		; Move border colour into the sprite colour.
	sta	$d027,x
	txa
	asl
	tay
	lda	#TAKEOVER_FIRST_SPRITEX
	sta	$d000,y		; X-position
	lda	tmp1
	sta	$d001,y		; Y-position
	clc
	adc	#42		; The Life, Universe and Everything.
	sta	tmp1
	inx
	cpx	#8
	bne	l2
	;; Prepare sprite buffer.
	ldx	#0
l1:	lda	#$ff
	sta	TAKEOVER_SPRITEBUFFER,x
	inx
	lda	#0
	sta	TAKEOVER_SPRITEBUFFER,x
	inx
	sta	TAKEOVER_SPRITEBUFFER,x
	inx
	cpx	#$3f		; Filled the whole sprite?
	bne	l1
	;; Configure sprites
	lda	#%00011111	; Only five sprites are needed.
	sta	$d015		; Enable the five sprites.
	lda	#$ff
	sta	$d017		; All sprites are double height.
	lda	#0
	sta	$d01d		; All sprites have normale width.
	sta	$d010		; All MSBs are cleared.
	lda	#121
	sta	skip_frames
	sta	takeover_curtain_left2righ ; nonzero means we are busy
	lda	#<TAKEOVER_SCREENBASE
	sta	topleftscreen
	lda	#>TAKEOVER_SCREENBASE
	sta	topleftscreen+1
	rts
.endproc

.proc	move_sprites_right
	.repeat	5,I
	inc	$d000+2*I
	.endrepeat
	lda	$d000		; At position $100?
	bne	out
	lda	#$ff		; Yes, set MSBs.
	sta	$d010
out:	rts
.endproc

.proc update_takeover_curtain_left2right
	lda	takeover_curtain_left2righ
	bne	do_something
	rts
do_something:
	lda	skip_frames
	beq	noskip
	dec	skip_frames
	rts
noskip:	lda	#TAKEOVER_DISSOLVE_SPEED
	sta	skip_frames	; Reset the frame counter.
	jsr	move_sprites_right
	lda	$d000		; X-position of the sprites; there all the same, you know.
	and	#%00000111	; Only the lowest three bits are interesting, is it a multiple of eight?
	bne	out
	lda	topleftscreen
	sta	ptr1
	lda	topleftscreen+1
	sta	ptr1+1
	ldx	#25-1		; 25 lines
	stx	linecounter
	ldy	#0		; Index to zero
l1:	lda	#$A0		; Inverse space.
	sta	(ptr1),y	; Store it to the screen ram.
	lda	ptr1+1		; Screen ram pointer HI.
	pha			; Store on stack.
	clc			; Adjust for colour ram.
	adc	#$d8-(>TAKEOVER_SCREENBASE)
	sta	ptr1+1
	lda	$d020		; Get border colour.
	sta	(ptr1),y	; Fill colour ram with border colour.
	pla			; Restore screen ram pointer.
	sta	ptr1+1
	lda	ptr1		; Increment pointer to next screen line.
	clc
	adc	#40
	sta	ptr1
	lda	ptr1+1
	adc	#0
	sta	ptr1+1
	dec	linecounter	; Count/decrement number of lines.
	bpl	l1
	inc	topleftscreen
	lda	topleftscreen
	cmp	#40		; Screen filled?
	bne	noincrement
	lda	#0
	sta	takeover_curtain_left2righ
noincrement:
out:
	rts
.endproc
