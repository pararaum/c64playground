	STUBDEST = $33c		; Cassette buffer.
	DATADEST = $0801

	.export	data_start
	.export	data_start_offset = data_start-$801
	.export	data_end
	.export	data_end_offset = data_end-$801
	.export	data_len
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
	.word	data_end
	.byte	$9e,"2061"
next:	.res	3
	sei
	lda	#$34
	sta	1
	ldx	#0
l1:	lda	stub,x
	sta	STUBDEST,x
	inx
	cpx	#stublen
	bne	l1
	ldx	#0
logoloop:
	.repeat	4,I
	 lda	logo+I*$100,x
	 sta	$400+I*$100,x
	.endrepeat
	dex
	bne	logoloop
	jmp	STUBDEST
logo:	.incbin	"logo.80x50.raw"
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
	sta	DATADEST,x
	dptr2=*-2
	inx
	bne	sl3
	inc	dptr2+1
	inc	sptr2+1
	bne	sl3
	lda	#$37		; Defaul, ROMs & I/O on.
	sta	1
stubcli:
	cli
	jmp	*
stubjmp = *-2
	.reloc
stublen = *-stub

data_start:
data_end:
data_len = data_end-data_start
