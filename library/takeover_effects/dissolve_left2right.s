
	.include	"t7d/kernal.i"
	.importzp	ptr1
	.import	TAKEOVER_SCREENBASE

	.export	init_takeover_dissolve_left2right
	.export	update_takeover_dissolve_left2right
	.export	takeover_dissolve_left2righ

TAKEOVER_DISSOLVE_SPEED=0

	.bss
skip_frames:	.res	1
charidx:	.res	1	; Index to the current character.
linecounter:	.res	1	; Counter for the lines to process.
topleftscreen:	.res	2	; Pointer to the top left screen corner.
takeover_dissolve_left2righ:
	.res	1	; Flag if effect is acting (0=finished).

	.rodata
dissolve_chars:
	;; 	.byte	$7e,$ff,$e6,$fc,$a0,0
	.byte	$20
	.byte	$74,$75,$61
	.byte	$f6,$ea
	.byte	$a0
	.byte	0

	.code
init_takeover_dissolve_left2right:
	lda	#$8e
	sta	takeover_dissolve_left2righ
	jsr	CHROUT
	lda	#121
	sta	skip_frames
	lda	#0
	sta	charidx
	lda	#<TAKEOVER_SCREENBASE
	sta	topleftscreen
	lda	#>TAKEOVER_SCREENBASE
	sta	topleftscreen+1
	rts


.proc update_takeover_dissolve_left2right
	lda	takeover_dissolve_left2righ
	bne	do_something
	rts
do_something:
	lda	skip_frames
	beq	noskip
	dec	skip_frames
	rts
noskip:	lda	#TAKEOVER_DISSOLVE_SPEED
	sta	skip_frames
	lda	topleftscreen
	sta	ptr1
	lda	topleftscreen+1
	sta	ptr1+1
	ldx	#25-1		; 25 lines
	stx	linecounter
	ldy	#0		; Index to zero
	ldx	charidx		; dissolve character index
l1:
	lda	dissolve_chars,x ; Get current dissolve character.
	sta	(ptr1),y	; Store it to the screen ram.
	lda	ptr1+1		; Screen ram pointer HI.
	pha			; Store on stack.
	clc			; Adjust for colour ram.
	adc	#$d8-(>TAKEOVER_SCREENBASE)
	sta	ptr1+1
	lda	$d020
	sta	(ptr1),y
	pla
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
	inc	charidx		; Use next char in array.
	ldx	charidx		; Check if char is zero which means go to next column.
	lda	dissolve_chars,x
	bne	noincrement
	sta	charidx		; Reset character index to zero.
	inc	topleftscreen
	lda	topleftscreen
	cmp	#40		; Screen filled?
	bne	noincrement
	lda	#0
	sta	takeover_dissolve_left2righ
noincrement:
	rts
.endproc
