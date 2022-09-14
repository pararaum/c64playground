;===================================================================================================
; Water Rising takeover effect test program
; Code by Wil 2022
; Version 0.1
; License: The Unlicense
;
; assemble with 
; cl65 water_rising_test.s water_rising.s -lib LAMAlib.lib -C c64-asm.cfg -o water_rising_test.prg
;===================================================================================================

.include "LAMAlib.inc"
.include "water_rising.i"

TAKEOVER_SCREENBASE=$400

	makesys 2022," vintage computing carinthia"

	jsr init_takeover_water_rising
	sei
	pokew $314,isr
	set_raster_irq 0
	cli
wait:	
	lda takeover_water_rising
	bne wait

	waitkey
	jsr 64738
 

isr:
	jsr update_takeover_water_rising
	asl $d019
	jmp $ea81