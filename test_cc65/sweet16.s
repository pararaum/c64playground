;;; cl65 --start-addr 0x600 -C c64-asm.cfg --asm-include-dir ../library/ sweet16.s ../library/libT7D.lib
;;; docker run -u 1000:uucp -v "$PWD":/host -w /host --rm -it vintagecomputingcarinthia/c64build prepender64 -J --copy-eor sweet16

;;; http://www.6502.org/source/interpreters/sweet16.htm
;;; https://en.wikipedia.org/wiki/SWEET16
;;; https://github.com/BruceMcF/Sweeter16
;;; https://github.com/Silverune/Sweet16
;;; https://ia600305.us.archive.org/11/items/Apple2_Woz_Sweet16/Apple2_Woz_Sweet16.pdf

	.include	"t7d/pseudo/sweet16.i"

; 	.zeropage
; SWEET16_REGSPACE:	.res	32

	.export	SWEET16_REGSPACE=$39 ; A good value...

	.code
	bit	brkvector
	lda	#<brkvector
	sta	$316
	lda	#>brkvector
	sta	$317
	jsr	sweet16_default
	.pushcpu
	.setcpu	"sweet16"
	set	r0,$93		; Clear screen.
	bk
	set	r1, $a000
	set	r2, $400
	set	r3, $200
l16:	ld	@r1
	st	@r2
	dcr	r3
	bnz	l16
	rtn
	.popcpu
	inc	$d020
	jmp	$FF8A		; RESTOR

brkvector:
	inc	$d020
	lda	SWEET16_REGSPACE
	jsr	$FFD2
;	tsx
;	;; Decrement the return address, as BRK is actually a two-byte instruction (kind of).
;	lda	$105,x		; LO of return is zero?
;	bne	nounderflow
;	dec	$106,x		; Decrement HI of return.
;nounderflow:
;	dec	$105,x		; Decrement LO of return.
	jmp	$ea81
