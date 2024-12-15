; -*- mode: asm -*-

	;; Pointer to the theatre text screen.
THEATRE_TEXT = $C000
THEATRE_TEXT_SIZE = 1000
	;; Sprite pointer array.
THEATRE_SPRT_PTR = THEATRE_TEXT + 1024-8
 	;; Initial colour information to be copied to colour RAM.
THEATRE_COLR = $C400
THEATRE_COLR_SIZE = 1000
 	;; Beginning of sprite data.
THEATRE_SPRT = $C800
THEATRE_SPRT_SIZE = $1000
	;; Beginning of sprite data (alternative memory location).
THEATRE_ASPR = $D000
THEATRE_ASPR_SIZE = $0800
 	;; Chargen data.
THEATRE_FONT = $D800
THEATRE_FONT_SIZE = $800
 	;; Optional graphics area.
THEATRE_GPHX = $E000
THEATRE_GPHX_SIZE = 8000
 	;; Optional space text area, colours.
THEATRE_SPARE_COLS = $fc00
 	;; Optional space text area, characters.
THEATRE_SPARE_TEXT = $f800
