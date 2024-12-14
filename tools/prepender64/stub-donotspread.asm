
	* = $0801
	STUBDEST=$0100
	DATADEST=$0801

	.word	next
	.word	main
	.text	$9e,"2077:",$8f," DEBUG VERSION"
next:	.byte	0,0,0
main:	sei
	ldx	#$ff
ploop:	inx
	lda	text,x
	lsr
	php
	jsr	$ffd2
	plp
	bcc	ploop
	lda	#$34		; Only RAM configuration.
	sta	1
	ldx	#0
l1:	lda	stub,x
	sta	STUBDEST,x
	inx
	cpx	#stublen
	bne	l1
	jmp	STUBDEST
text:	.shiftl	"DEBUG VERSION!",13,"DO NOT SPREAD!"

	;; This stub does the actual work.
stub:
	.logical	STUBDEST
	ldx	#0
sl2:	dec	sptr+1
	dec	dptr+1
sl1:	lda	data_end,x
	sptr=*-2
	sta	@w $0,x
	dptr=*-2
	dex
	bne	sl1
	lda	sptr+1
	cmp	#7
	bne	sl2
sl3:	lda	@w 0+(-data_len)&$FFFF,x
	sptr2=*-2
	eor	#0
	stubeor=*-1
	sta	DATADEST,x
	dptr2=*-2
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
	.endlogical
stublen = *-stub

data_start:
data_end:
data_len = data_end-data_start

stub10000_minus_datalen_offset = sptr2-STUBDEST+stub-$0801
stubjump_to_offset = stubjmp-STUBDEST+stub-$801
stubsptr_data_end_offset = sptr-STUBDEST+stub-$801
stubdptrDATADEST_offset = dptr2-STUBDEST+stub-$801
stubeorvalue_offset = stubeor-STUBDEST+stub-$801
