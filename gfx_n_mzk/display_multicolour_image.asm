;;; xa -M -I ../include/ -l /dev/stdout display_multicolour_image.asm
;;; Some inspiration taken from https://github.com/micheldebree/retropixels/blob/master/src/c64/Koala.asm
;;; For the file format: https://codebase64.org/doku.php?id=base:c64_grafix_files_specs_list_v0.03
IMAGE_DESTINATION = $2000
SCREENPTR = $c800
BITMAPPTR = $e000

	.word	$0801
	*=$0801

	;; Just a simple BASIC header and 2064 is $0810.
	.word	end_of_basic	; 0801
	.word	main		; 0803
	.byte	$9e,$20,"(2064)" ; 0805
end_of_basic:
	.byte	0,0,0		; 080d

main:	jsr	enable_bitmap
	jsr	initialise_bitmap_and_screenptr
	lda	#<image		; Copy image data to BITMAPPTR.
	sta	$fc
	lda	#>image
	sta	$fd
	lda	#<BITMAPPTR
	sta	$fe
	lda	#>BITMAPPTR
	sta	$ff
	lda	#$2000/$100
	pha
	jsr	MEMCPY
	pla			; Clean up the stack.
	lda	#<(image+$1f40)	; Copy the colour data in the screen area.
	sta	$fc
	lda	#>(image+$1f40)
	sta	$fd
	lda	#<SCREENPTR
	sta	$fe
	lda	#>SCREENPTR
	sta	$ff
	lda	#1000/$100+1
	pha
	jsr	MEMCPY
	pla
	lda	#<(image+$2328)	; Copy the last colour information into the colour RAM.
	sta	$fc
	lda	#>(image+$2328)
	sta	$fd
	lda	#<$d800
	sta	$fe
	lda	#>$d800
	sta	$ff
	lda	#1000/$100+1
	pha
	jsr	MEMCPY
	pla
	lda	image+$2710	; Background colour is now copied to the background and border.
	sta	$d020
	sta	$d021
	clv			; Endless loop.
	bvc	*

enable_bitmap:
	lda	#$20		; Bitmap modus.
	ora	$d011
	sta	$d011
	lda	#$10		; Multicolour modus.
	ora	$d016
	sta	$d016
	rts

#include "memcpy.inc"
#include "bitmap_n_screen.inc"
	.dsb	IMAGE_DESTINATION-*
;;; This image was "ripped" from https://csdb.dk/release/?id=166642. This was #1 in the Nordlicht 2018 Oldschool Graphics competition.
image:	.bin	2,0,"deepsea.koala"
