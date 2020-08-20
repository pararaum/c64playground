
INITDR = $D042			; Load the BAM

	;;  Buffer #2 at $500 seems to be available...
	.word	$500
	;;  Then size...
	.byte	program_end-main

	* = $500
main:	.(
	lda	flag
	bne	jobmain
	lda	#1
	sta	flag
	jsr	INITDR
	;; Job code memory locations [Inside Commodore DOS, p. 219].
l2:	lda	track		; Track
	sta	$a		; Track for buffer #2.
	lda	#0		; Sector
	sta	$b		; Sector for buffer #2
	lda	#$e0		; Job code (EXECUTE in #2).
	sta	$02
l1:	lda	$02
	bmi	l1
	dec	track
	bne	l2
	lda	#0
	sta	flag
	jmp	INITDR
track:	.byte	39
	.)

jobmain:			; Should be at $500
	lda	$1c00		; Control Port
	ora	#8		; LED on
	sta	$1c00
	jsr	$fe0e		; Clear Track with $55
	lda	$1c00		; Control Port
	and	#%11110111	; LED off
	sta	$1c00
	jsr	$fe00		; Read mode.
	jmp	$fd9e		; OK and be gone.

flag:	.byte	0
program_end:
