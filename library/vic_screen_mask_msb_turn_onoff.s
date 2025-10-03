	.include	"t7d/vic/screen.i"

	.export	vic_screen_msb_turn_off
	.export	vic_screen_msb_turn_on

vic_screen_msb_turn_off:
	lda	#$7f
	jmp	vic_screen_mask_turn_off

vic_screen_msb_turn_on:
	lda	#$7f
	jmp	vic_screen_mask_turn_on
