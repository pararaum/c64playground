	STUBDEST = $400
	DATADEST = $0801

	;; This just the negative length of the data as a 16-bit value. len=1 is $FFFF
	.export	stub10000_minus_datalen_offset = sptr2-STUBDEST+stub-$801
	;; Offset to the destination address where the data is finally copied to, defaults to $801.
	.export	stubdptrDATADEST_offset = dptr2-STUBDEST+stub-$801
	;; Offset for the CLI, if interrupts should be disabled, write a NOP here.
	.export	stubcli_offset = stubcli-STUBDEST+stub-$801
	;; Modify this field to get the correct end of the data to be copied. The length of the data has to be *added* to this field.
	.export	stubsptr_dataend_offset = sptr-STUBDEST+stub-$801
	;; Offset for the JMP to the final code.
	.export	stubjmp_offset = stubjmp-STUBDEST+stub-$801

	.word	next
	.word	stublen
	.byte	$9e," ",$30+<((main/1000) .mod 10),$30+<((main/100) .mod 10),$30+<((main/10) .mod 10),$30+<(main .mod 10)
next:	.res	3
	.word	1
main:	sei
	lda	1
	pha
	lda	#$34
	sta	1
	ldx	#0
l1:	lda	stub,x
	sta	STUBDEST,x
	inx
	cpx	#stublen
	bne	l1
	jmp	STUBDEST

stub:
	.org	STUBDEST
	ldx	#0
sl2:	dec	sptr+1
	dec	dptr+1
sl1:	lda	data_end,x
	sptr=*-2
	sta	a:$0,x
	dptr=*-2
	dex
	bne	sl1
	lda	sptr+1
	cmp	#7
	bne	sl2
sl3:	lda	a:0+(-data_len)&$FFFF,x
	sptr2=*-2
	eor	stublfsrregister
	sta	DATADEST,x
	dptr2=*-2
	;; Update LFSR.
	lsr	stublfsrregister+1
	ror	stublfsrregister
	bcc	@noxor
	lda	stublfsrregister
	eor	stublfsrfeedback
	sta	stublfsrregister
	lda	stublfsrregister+1
	eor	stublfsrfeedback+1
	sta	stublfsrregister+1
@noxor:
	inx
	bne	sl3
	inc	dptr2+1
	inc	sptr2+1
	bne	sl3
	pla
	sta	1
stubcli:
	cli
	jmp	*
stubjmp = *-2
stublfsrregister:	.word	$F77D
stublfsrfeedback:	.word	$8117
	.reloc
stublen = *-stub

data_start:
data_end:
data_len = data_end-data_start
