	.importzp	ptr1

	.export	a_times_64,x_times_64,y_times_64

x_times_64:
	txa
	.byte	$24		; Bit zeropage, will skip the tya!
y_times_64:
	tya
a_times_64:
	sta	ptr1
	lda	#0
	sta	ptr1+1
	.repeat	6		; 2^6=64
	 asl	ptr1
	 rol	ptr1+1
	.endrepeat
	rts
