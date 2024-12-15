
	.export	busywait_frame_mp
	.export	_busywait_frame_mp=busywait_frame_mp

busywait_frame_mp:
	bit	$d011
	bmi	busywait_frame_mp
l2:	bit	$d011
	bpl	l2
	rts
