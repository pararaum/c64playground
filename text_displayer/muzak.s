
	.export muzak_init
	.export muzak_play

	.segment "MUZAK"
muzak_sid:
;;; 	.incbin "Vibrations.sid",$7c+2
	.incbin "Solix.sid",$7c+2
	muzak_init = muzak_sid
	muzak_play = muzak_sid+3
