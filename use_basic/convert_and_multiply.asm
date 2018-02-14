;;; Simple Program which uses the BASIC routines for number manipulations.

;;; LINKS
;;; - control characters: https://www.c64-wiki.de/wiki/Steuerzeichen
;;; - Fully documented BASIC disassembly: http://www.pagetable.com/c64rom/c64rom_sc.html
;;; - memory map: http://sta.c64.org/cbm64mem.html
;;; - floating point arithmetic: https://www.c64-wiki.com/wiki/Floating_point_arithmetic
;;; - floating point arithmetic: https://www.c64-wiki.de/wiki/Flie%C3%9Fkommaarithmetik
;;; - C64 kernal functions: http://sta.c64.org/cbm64krnfunc.html
;;; - kernal: https://www.c64-wiki.de/wiki/KERNAL
;;; - BASIC rom addresses: https://www.c64-wiki.de/wiki/BASIC-ROM
;;; - Zeropage: https://www.c64-wiki.de/wiki/Zeropage
;;; - mathematics: http://codebase64.org/doku.php?id=base:kernal_floating_point_mathematics

	.text
	* = $0801-2

	;; Convert FAC to string at $0100
	;; return A/Y points to string
	FOUT = $BDDD
	;; Convert A/Y to FAC
	GIVAYF = $B391

	.word	$0801
	.bin	0,0,"basic.stub"
	.dsb	$0A00-*
	jmp	init
	jmp	main
	jmp	syspara
	jmp	fini
	jmp	over
over:	brk

init:	ldx	#$09
	stx	$d021
	dex
	stx	$d020
	dex
	stx	$286
	lda	#147
	jsr	$ffd2
	;; Set user vector
	ldx	#<USR
	stx	$311
	ldx	#>USR
	stx	$312
	rts

USR:	rts

syspara:	jsr	$AEFD	;Comma?
	jsr	$B79E		;Get next value into X
	stx	$fe
	jsr	$AEFD
	jsr	$B79E
	stx	$fd
	rts

fini:	php
	sta	$fc
	stx	$fd
	sty	$fe
	lda	#$0d
	jsr	$FFD2
	lda	#$00
	ldy	$fc
	jsr	GIVAYF
	jsr	FOUT
	jsr	$ab1e
	lda	#$00
	ldy	$fd
	jsr	GIVAYF
	jsr	FOUT
	jsr	$ab1e
	lda	#$00
	ldy	$fe
	jsr	GIVAYF
	jsr	FOUT
	jsr	$ab1e
	pla
	tay
	lda	#$00
	jsr	GIVAYF
	jsr	FOUT
	jsr	$ab1e
	rts


main:	lda	#$40
	ldy	#$04
	jsr	GIVAYF		;Convert A/Y to FAC
	jsr	FOUT		;Convert FAC to string
	;; Now A/Y contains the address
	jsr	$ab1e		;Output string

	lda	#$40
	pha
	lda	#$42
	pha
	lda	#$0f
	pha
	jsr	convert24
	;; Clean up stack
	pla
	pla
	pla
	jsr	FOUT		;Output to $0100
	jsr	$ab1e		;Print on screen
	lda	#$ff
	pha
	pha
	pha
	pha
	jsr	convert32
	pla
	pla
	pla
	pla
	jsr	FOUT		;Output to $0100
	jsr	$ab1e		;Print on screen
	lda	#$ff
	pha
	pha
	lda	#$00
	pha
	jsr	convert24
	pla
	pla
	pla
	jsr	$bc0c		;FAC to ARG
	lda	#10
	jsr	$ba59
	jsr	$bb8f
	jsr	FOUT
	jsr	$ab1e
	lda	#13
	sta	$d020
	rts

convert32:	.(
	tsx
	lda	$0103,x
	sta	$62
	lda	$0104,x
	sta	$63
	lda	$0105,x
	sta	$64
	lda	$0106,x
	sta	$65
	lda	#$00
	sta	$66
	sta	$70
	lda	#128+32
	sta	$61
	jmp	$b8d2		;Nomalise
	.)

convert24:	tsx
	lda	#$00
	sta	$65
	lda	$0103,x
	sta	$62
	lda	$0104,x
	sta	$63
	lda	$0105,x
	sta	$64
	lda	#$00
	sta	$65
	sta	$66
	sta	$70
	lda	#128+24
	sta	$61
	jmp	$b8d2		;Nomalise
