	.include	"zeropage.inc"
	.include	"t7d/basic.i"
	.include	"t7d/kernal.i"
	.include	"t7d/stringfunctions.i"
	.include	"LAMAlib-macros16.inc"
	.include	"t7d/libt7d.i"
	.include	"t7d/vic/vicmacros.i"

;;; Input $ABF9... [D. Heeb, Compute!'s VIC-20 and Commodore 64 Tool Kit: BASIC, 1984, 341.]

	.rodata
hellotext:
	.byte	$9b, $93	; Light grey, clear screen.
	.byte	$d,"calculate the conway sequence",$d
	.byte	$d
	.res	40,$2d
	.byte	"code:  pararaum",$d
	.byte	"font:  damieng",$d
	.byte	"music: nordischsound",$d
	.res	40,$2d
	.byte	$d
	.asciiz	"initial string"

	.data
example:	.asciiz	"5551"

	.BSS
buffer1:	.res	15988
buffer2:	.res	15988

	.segment	"EXEHDR"
	lda	#0
	sta	$d020
	sta	$d021
	jsr	$1000
	sei
	SetIRQ314Pointer	irq
	cli
	SetChargenAddress $800
	jsr	main
	jmp	64738

.proc	irq
	jsr	$1003
	jmp	$ea31
.endproc

;;; Input: ptr1=pointer to sequence
;;; Output: A=sequence length, X=byte, ptr1=points to next byte.
.proc	count
	LDY	#0
	lda	(ptr1),y
	tax
loop:	iny
	cmp	(ptr1),y
	beq	loop
	tya
	clc
	adc	ptr1
	sta	ptr1
	lda	#0
	adc	ptr1+1
	sta	ptr1+1
	tya
	rts
.endproc

.proc	copy101_to_ptr2
	ldy	#0
loop:	lda	$101,y
	beq	out
	sta	(ptr2),y
	iny
	bne	loop
out:	tya
	rts
.endproc

;;; Input: ptr1=input sequence ptr2=output sequence
	.code
.proc	conway_step
	jsr	count
	stx	tmp1		; Keep byte safe. tmp1=current value.
	cpx	#0
	beq	out
	;; Convert A to number.
	jsr	BASIC_FAC1_WITH_UINT8
	jsr	BASIC_FLOAT_OUT	; String is at $100, A=0, Y=1.
	;; 	jsr	STROUT
	;; Write number to ptr2, use BASIC.
	jsr	copy101_to_ptr2
	clc
	adc	ptr2
	sta	ptr2
	lda	ptr2+1
	adc	#0
	sta	ptr2+1
	;; Write X to ptr2.
	lda	tmp1
	ldy	#0
	sta	(ptr2),y
	inc16	ptr2
	jmp	conway_step
out:	lda	#0
	tay
	sta	(ptr2),y
	rts
.endproc

.proc	output_long_string_lf
	jsr	output_long_string
	lda	#13
	jsr	CHROUT
	jmp	CHROUT
.endproc

.proc	input_data
	ldax	#hellotext
	jsr	output_long_string
	jsr	$ABF9
	ldax	#$200
	stax	ptr1
	jsr	output_long_string_lf
	ldax	#buffer1
	stax	ptr2
	jsr	conway_step
	ldax	#buffer1
	jsr	output_long_string_lf
	rts
.endproc

	.code
main:	jsr	input_data
mainloop:
	ldax	#buffer1
	stax	ptr1
	ldax	#buffer2
	stax	ptr2
	jsr	conway_step
	ldax	#buffer2
	jsr	output_long_string_lf
	;;
	ldax	#buffer2
	stax	ptr1
	ldax	#buffer1
	stax	ptr2
	jsr	conway_step
	ldax	#buffer1
	jsr	output_long_string_lf
	jmp	mainloop
