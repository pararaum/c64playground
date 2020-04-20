;;; -*- mode:asm -*-
;;; This include file can be used to have a simple setup for the screen
;;; and the bitmap pointers. You must define SCREENPTR and BITMAPPTR
;;; before including this file.
;;; Warning! Setting the screen pointer within the range of $9000 to
;;; $9fff will read the screen information from the character ROM! Not
;;; very useful.
;;;
;;; Make sure that both are in the same bank. Banks:
;;;
;;; Bank no.	Bit pattern ($DD00)	Address				ROM chars available?
;;; 0		xxxxxx11		0–16383		$0000–$3FFF	Yes, at $1000–$1FFF
;;; 1		xxxxxx10		16384–32767	$4000–$7FFF	No
;;; 2		xxxxxx01		32768–49151	$8000–$BFFF	Yes, at $9000–$9FFF
;;; 3		xxxxxx00		49152–65535	$C000–$FFFF	No

;;; https://www.c64-wiki.com/wiki/Page_208-211
;;; $D018 	53272 	Register 	VM13 	VM12 	VM11 	VM10 	CB13 	CB12 	CB11 	- 	Memory pointers

;;; I/O: -
;;; All registers are preserved.
initialise_bitmap_and_screenptr:
	pha
	lda #(((SCREENPTR & $3FFF)/$0400) << 4) | (((BITMAPPTR & $3FFF)/$0800) << 1)
        sta $d018
        lda $dd00
        and #%11111100
        ora #(BITMAPPTR ^ $FFFF) >> 14
        sta $dd00
	pla
        rts
        
