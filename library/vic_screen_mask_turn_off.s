	.include	"t7d/vic/screen.i"

	.export	vic_screen_mask_turn_off

vic_screen_mask_turn_off:
	and	$d011
	and	#%11101111
	sta	$d011
	rts
