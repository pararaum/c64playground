
	.export	busywait_frame_pm
	.export	_busywait_frame_pm=busywait_frame_pm

busywait_frame_pm:
	bit	$d011
	bpl	busywait_frame_pm
l2:	bit	$d011
	bmi	l2
	rts
