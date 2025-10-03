	.include	"t7d/vic/screen.i"

	.export	vic_screen_mask_turn_on

vic_screen_mask_turn_on:
	and	$d011
	ora	#%00010000
	sta	$d011
	rts
