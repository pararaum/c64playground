; This currently does not work. While linking the ZP segments are not merged. Doing it by hand is no option.

	.zero
a_zp_var:	.byte	00
a_2nd_zp:	.dsb 10
a_3rd_zp:	.byte $ff

	.data

a_data_text:	.asc	"HELLO WORLD!",0

	.text
loop:
	ldx	#0
l15:	lda	a_data_text,x
	beq	out
	jsr	$ffd2
	inx
	jmp	l15
out:	nop
	stx	a_zp_var
	nop
	jsr	func1
	jsr	func2
	jmp	loop
