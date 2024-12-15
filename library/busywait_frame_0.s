	.macpack generic

	.export	busywait_frames_0

	.bss
busywait_counter:
	.res	1

	.code
busywait_frames_0:
	sta	busywait_counter
l1:	bit	$d011		; Wait for rasterline < 256
	bmi	l1
l2:	lda	$d012		; Wait for rasterline = 0
	bne	l2
	dec	busywait_counter
	bne	l1
	rts
