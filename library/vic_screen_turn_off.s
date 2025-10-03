	.include	"t7d/vic/screen.i"

	.export	vic_screen_turn_off

vic_screen_turn_off:
	and	#%11101111
	sta	$d011
	rts
