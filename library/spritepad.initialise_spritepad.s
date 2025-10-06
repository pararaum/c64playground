	.include	"t7d/sprite/spritepad.i"
	.include	"t7d/sprite/sprite.i"
	.include	"t7d/stackmacros.i"
	.include	"LAMAlib.inc"
	.include	"zeropage.inc"

;;; Information about the format:
;;; https://csdb.dk/forums/?roomid=7&topicid=125812
;	
;	v2:
;	
; 0	3 bytes   - magic "SPD"
; 3	1 byte    - version (1)
; 4	1 byte    - number of sprites - 1
; 5	1 byte    - number of animations - 1
; 6	1 byte    - 0-3 background
; 7	1 byte    - 0-3 multicolour 1
; 8	1 byte    - 0-3 multicolour 2
;	
;	repeated for each sprite:
;	63 bytes  - sprite data
;	1 byte    - flags 0-3 colour, 4 overlay, 7 multi
;	
;	animation settings split into 4 arrays:
;	n bytes   - animation starts
;	n bytes   - animation ends
;	n bytes   - timers
;	n bytes   - flags 4 ping-pong, 5 overlay, 7 valid

	.export	spritepad_copy8_spritedata
	.export	spritepad_setup_vic
	.export	spritepad_initialise_spritepad

	.bss
spritepad_paddataptr:	.res	2 ; Pointer to the current spritepad.
spritepad_sprdataptr:	.res	2 ; Pointer to the current sprite data (9 bytes later).
spritepad_destination_sprbuf:	.res	2 ; Pointer to the destination spritebuffer (sprite 0).

	.code
.proc	spritepad_initialise_spritepad
	stax	spritepad_paddataptr
	stax	ptr1
	stax	spritepad_sprdataptr
	inc16	spritepad_sprdataptr, SPRITEPADDATAOFFSET
	PullStoreStackptrLOCAL
	PullWordHL	spritepad_destination_sprbuf
	RetrievePushStackptrLOCAL
	ldy	#4
	lda	(ptr1),y
	rts
.endproc

.proc	spritepad_setup_vic
	ldax	spritepad_paddataptr ; Copy spritepad data pointer to ptr1.
	stax	ptr1
	ldy	#7
	lda	(ptr1),y	; Multicolour 1
	sta	$d025
	ldy	#8
	lda	(ptr1),y	; Multicolour 2
	sta	$d026
	ldy	#6
	lda	(ptr1),y
	rts
.endproc

.proc	spritepad_copy8_spritedata
	stax	SPRITENUMLISTPTR
	ldax	spritepad_destination_sprbuf
	stax	ptr2		     ; Pointer to the spritebuffer.
	ldx	#0
l1:
	lda	*,x
	SPRITENUMLISTPTR=*-2
	sta	ptr1+1		; HI!
	lda	#0		; LO of offset.
	;; Actually multiply by 64
	lsr	ptr1+1
	ror
	lsr	ptr1+1
	ror
	sta	ptr1
	;; Add offset to source.
	clc
	adc	spritepad_sprdataptr
	sta	ptr1
	lda	ptr1+1
	adc	spritepad_sprdataptr+1
	sta	ptr1+1
	;;  Copy 63 bytes.
	ldy	#63-1
l2:
	lda	(ptr1),y
	sta	(ptr2),y
	dey
	bpl	l2
	;; After 63 Bytes additional information about the sprite.
	ldy	#63		; Last byte in sprite data.
	lda	(ptr1),y
	php			   ; Store processor status, bit 7 = MC = negative!
	sta	$d027,x	   ; Individual colour of sprite.
	lda	SPRITE_HIGH_BITS,x ; Bit of sprite.
	eor	#$FF		   ; Invert
	and	$d01c		   ; Mask bit out of multicolour register.
	plp			   ; Get status back
	bpl	nomulticol
	ora	SPRITE_HIGH_BITS,x
nomulticol:
	sta	$d01c	   ; Write next multicolour information to VIC.
	inc16	ptr1, 64	; Advance to the next source sprite buffer.
	inc16	ptr2, 64	; Advance to the next destination sprite buffer.
	inx
	cpx	#8
	bne	l1
	rts
.endproc
