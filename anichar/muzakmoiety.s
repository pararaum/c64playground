
	.export	muzak_play
	.export	muzak_init
	
	.segment	"MUZAK"
muzak_init:
	.incbin	"Back_to_Basics.sid",$7c+2

	muzak_play := muzak_init+3
