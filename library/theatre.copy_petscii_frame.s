	.include	"t7d/theatre/consts.i"
	.include	"t7d/memoryconfig.i"
	.include	"LAMAlib-macros16.inc"
	.include	"zeropage.inc"

	.export	_theatre_copy_frame2text

_theatre_copy_frame2text:
	stax	ptr1		; Store frame pointer into ptr1
	lda	1		; Get old memory config.
	pha
	memoryconfig_io
	ldy	#0
	lda	(ptr1),y
	sta	$d020
	iny			; Y=1
	lda	(ptr1),y
	sta	$d021
	lda	ptr1		; LO of frame
	addax	#2
	stax	ptr1		; Pointer to first character
	addax	#1000
	stax	ptr2		; Pointer to first colour information
	dey			; Y=0
	lda	#>THEATRE_TEXT
	sta	theatre_text_pointerHI
	lda	#$d8
	sta	colour_pointerHI
	ldx	#4-1
loop:	lda	(ptr1),y
	sta	THEATRE_TEXT,y
	theatre_text_pointerHI=*-1
	lda	(ptr2),y
	sta	$d800,y
	colour_pointerHI=*-1
	dey			; Must decrement otherwise last turn trick below does not work.
	bne	loop
	cpx	#0		; Is this the last turn?
	bne	not_last
	;; Y=0 here!
	lda	(ptr1),y	; Get 768th character.
	sta	THEATRE_TEXT+768
	lda	(ptr2),y	; Get 768th colour.
	sta	$d800+768
not_last:
	inc	ptr1+1
	inc	ptr2+1
	inc	theatre_text_pointerHI
	inc	colour_pointerHI
	dex
	beq	last_turn	; If X=0 this is the last turn of the loop.
	bpl	loop
	pla
	sta	1
	rts
last_turn:
	;; We should only copy 1000 bytes (40*25) as the last eight bytes contain the sprite pointers and we do not want to overwrite them.
	ldy	#256-24		; Skip the last 24 bytes.
	bne	loop		; Always taken!
	;; As we enter the loop the decrementing part of the inner loop will only copy 232 bytes.
