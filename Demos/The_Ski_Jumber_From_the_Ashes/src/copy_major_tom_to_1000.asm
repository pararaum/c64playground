
	*=$E000

PLAYERLOUDNESS=$101b		; Seems to contain the loudness...

	sei
	lda	$fffe
	pha
	lda	$ffff
	pha
	lda	#<irq
	sta	$fffe
	lda	#>irq
	sta	$ffff
	cli
wait:	lda	loudness
	bne	wait

	sei
	pla
	sta	$ffff
	pla
	sta	$fffe

	ldx	#$14
	lda	#0
	sta	$d418
l2:	sta	$d400,x
	dex
	bpl	l2

	ldx	#0
loop:	lda	thesid,x
	SIDPTR=*-2
	sta	$1000,x
	MUSPTR=*-2
	dex
	bne	loop
	inc	SIDPTR+1
	inc	MUSPTR+1
	lda	SIDPTR+1
	cmp	#>thesid_end+1
	bne	loop
	lda	#0
	jsr	$1000
	cli
	rts


irq:	pha
	asl	$d019
	jsr	$1003
	lda	loudness
	lsr
	lsr
	lsr
	sta	PLAYERLOUDNESS
	dec	loudness
	pla
	rti


loudness:	.byte	15*6

thesid:	.binary	"../assets/major_tom.sid",$7c+2
thesid_end:
