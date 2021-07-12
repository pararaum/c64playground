
	.include	"kernal.i"
	.include	"libt7d.i"
	.include	"memoryfunctions.i"
	.include	"vicmacros.i"

	;; Import the SID song begin and end (pointer after last byte).
	.import	SIDSONG_BEGIN
	.import	SIDSONG_END
	.import	__MAIN_START__

	.export	ptr1,ptr2

	.zeropage
	;; Generic pointer.
ptr1:	.res	2
ptr2:	.res	2
	;; Pointer to the sidsong
sidsongptr:	.res	2

	.segment	"LOADADDR"
	.word	__MAIN_START__

	.segment	"EXEHDR"
	jmp	_start

	.segment	"ONCE"
output_ascii:
	;; Poor man's conversion of ASCII vs. PETSCII.
	cmp	#$41
	bcc	@no
	EOR	#$20
@no:
	jmp	CHROUT


_output_string20:
	tya
	clc
	adc	#$20
	sta	@smc
@l1:	lda	(sidsongptr),y
	beq	@end
	jsr	output_ascii
	iny
	cpy	#$20		; Maximum length reached?
	@smc=*-1		; Self-modifying code!
	bne	@l1
@end:
	rts

;;; Output the songname to the screen.
;;; Input: sidsongptr=pointer to the song
;;; Output: -
;;; Modifies: A,X,Y
output_songname:
	ldy	#$16		; Songname starts there.
	jmp	_output_string20

output_author:
	ldy	#$36		; Author starts there.
	jmp	_output_string20
	
output_released:
	ldy	#$56
	jmp	_output_string20

_start:
	lda	$2a6		; NTSC/PAL? PAL=1
	lsr			; Put LSB into Carry.
	ror			; Move into MSB
	ora	$dd0e		; CIA2 Timer A control register (Bit#7 TOD speed).
	sta	$dd0e		; Bit#7=1 if 50 Hz.
	lda	#1		; Turn border and background to white.
	sta	$d020
	sta	$d021
	lda	#<SIDSONG_BEGIN	; Set the sidsong pointer to the address of the sidsong.
	sta	sidsongptr
	lda	#>SIDSONG_BEGIN
	sta	sidsongptr+1
	ldy	#12
	ldx	#8
	clc
	jsr	PLOT
	jsr	output_songname
	ldy	#12
	ldx	#10
	clc
	jsr	PLOT
	jsr	output_author
	ldy	#12
	ldx	#12
	clc
	jsr	PLOT
	jsr	output_released
	lda	#6
	jsr	_fill_colour_ram
	SetChargenAddress	$800
	jmp	main

;;; Get the offset to the actual song data. WARNING! This currently works only with PSID >= V2.
;;; Input: SIDSONG_BEGIN
;;; Output: A/X=pointer to the song data (load address is located there)
;;; Modifies: A,X
get_song_offsets:
	lda	#<SIDSONG_BEGIN
	ldx	#>SIDSONG_BEGIN
	clc			; There the song begins.
	adc	#$7c
	bcc	@no_overflow
	inx
@no_overflow:
	rts


;;; Copy the song to the address it belongs to.
;;; Input: -
;;; Output: -
;;; Modifies: A,X,Y,ptr1,ptr2
copy_song:
	jsr	get_song_offsets ;A/X=offset to song.
	sta	ptr1
	stx	ptr1+1
	ldy	#0
	lda	(ptr1),y	; destination LO
	sta	ptr2
	iny
	lda	(ptr1),y	; destination HI
	sta	ptr2+1
	lda	ptr1		; Skip the load address.
	clc
	adc	#2
	sta	ptr1
	lda	ptr1+1
	adc	#0
	sta	ptr1+1
	lda	#<(SIDSONG_END-SIDSONG_BEGIN)
	ldx	#>(SIDSONG_END-SIDSONG_BEGIN)
	jmp	memcpy_up

setup_irq:
	sei
	lda	#51
	sta	$d012		; Rasterline for IRQ
	lda	$d011		; MSB of rasterline
	and	#$7f
	sta	$d011
	lda	#1
	sta	$d01a		; Enable raster irq
	SetIRQ314Pointer	irq_routine
	cli
	rts
	

	.bss

	.code
irq_routine:
	lda	$dd09		; seconds
	pha
	and	#$f
	adc	#$30
	sta	$0400+999
	pla
	lsr
	lsr
	lsr
	lsr
	clc
	adc	#$30
	sta	$0400+998
	lda	$dd0a		; Minutes
	pha
	and	#$f
	adc	#$30
	sta	$0400+996
	pla
	lsr
	lsr
	lsr
	lsr
	clc
	adc	#$30
	sta	$0400+995
	lda	#':'
	sta	$0400+997	; Store the ':'
	inc	$d020
	asl	$d019		; Acknowledge IRQ
	jsr	$1003
	dec	$d020
	jmp	EXITIRQ
	.code
main:
	jsr	get_song_offsets
	jsr	copy_song
	jsr	_disable_cia_irq
	lda	#0
	jsr	$1000
	jsr	setup_irq
	lda	#0		; Clear CIA1 clock.
	sta	$dd0a
	sta	$dd09
	sta	$dd08
	jmp	*
