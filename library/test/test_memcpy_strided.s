
	.include	"t7d/stackmacros.i"
	.include	"t7d/memoryfunctions.i"
	.include	"t7d/crc8.i"
	.import		pushax, _write, _sprintf, _strlen
	.import		exit
	.importzp	ptr1, ptr2
	.forceimport	__STARTUP__

	.export _main, CRCTABLE

	.data
text:	.byte	"Mismatch!",10,0
text_end:

numberset:
	.repeat	256,I
	.byte	I
	.endrepeat

format:	.byte	"%04x",10,0

	.bss
buffer:	.res	80
CRCTABLE:	.res	256
numberdest:	.res	256

	.macro	Pushaddr addr
	lda	#<(addr)
	ldx	#>(addr)
	jsr	pushax
	.endmacro

	.code
printbyte:	pha
	Pushaddr	buffer
	Pushaddr	format
	ldx	#0
	pla
	jsr	pushax
	ldy	#6		; Six bytes on C stack.
	jsr	_sprintf
	lda	#1		; fd
	ldx	#0
	jsr	pushax
	Pushaddr	buffer	; buffer
	lda	#5		; length
	ldx	#0
	jmp	_write

do_crc:
	ldy	#0
@l1:
	lda	numberdest,y
	jsr	update_crc
	iny
	bne	@l1
	pha
	jsr	printbyte
	pla
	rts

	.data
varI:	.byte	0

	.code
printdestination:
@l1:	ldx	varI
	;; 	lda	numberset,x
	lda	numberdest,x
	jsr	printbyte
	inc	varI
	bne	@l1
	rts


	.code
_main:
	jsr	make_crctable
	jsr	do_crc
	cmp	#$D7		; Start with empty buffer.
	beq	@ok_empty
	lda	#$ff
	rts
	@ok_empty:
	lda	#16
	sta	memcpy_strided_srcstride
	lda	#4
	sta	memcpy_strided_srcwidth
	sta	memcpy_strided_srcheight
	sta	memcpy_strided_dststride
	lda	#<numberset
	sta	ptr1
	lda	#>numberset
	sta	ptr1+1
	lda	#<numberdest
	sta	ptr2
	lda	#>numberdest
	sta	ptr2+1
	jsr	memcpy_strided

	;; 	jsr	printdestination
	jsr	do_crc
	cmp	#$eb
	beq	@ok_copy
	lda	#$FF
	rts
	@ok_copy:
	lda	#0
	rts
