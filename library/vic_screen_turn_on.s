	.include	"t7d/vic/screen.i"

	.export	vic_screen_turn_on

vic_screen_turn_on:
	ora	#%00010000
	ora	$d011
	rts
