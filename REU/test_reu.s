;;; cl65 -t c64 -C c64-asm.cfg -u __EXEHDR__ test_reu.s
;;; https://codebase64.org/doku.php?id=base:reu_programming
;;; https://www.c64-wiki.com/wiki/REU
;;; x64 -autostart test_reu
;;;
;;; truncate -s 4M /tmp/reu.bin
;;; x64 -cartreu /tmp/reu.bin -reuimagerw -reu -autostart test_reu

	REUSTATUS   = $DF00
	REUCOMMAND  = $DF01
	REUC64BASE  = $DF02
	REUREUBASE  = $DF04
	REUTRANSLEN = $DF07
	REUIRQMASK  = $DF09
	REUCONTROL  = $DF0A

	BLKSZ = 4096

	.bss
spare:	.res	BLKSZ
spare_end:

	.zeropage
ptr:	.res	2

	.code
	jmp	_main

fill:
	lda	#<spare
	sta	ptr
	lda	#>spare
	sta	ptr+1
	ldy	#0
@l1:	tya
	sta	(ptr),y
	iny
	bne	@l1
	inc	ptr+1
	lda	#>(spare_end)
	cmp	ptr+1
	bne	@l1
	rts

	.code
_main:	jsr	fill
	;; Copy into REU.
	lda	#<spare		; C64 source.
	sta	REUC64BASE
	lda	#>spare
	sta	REUC64BASE+1
	lda	#0		; REU destination address.
	sta	REUREUBASE
	sta	REUREUBASE+1
	sta	REUREUBASE+2
	lda	#<BLKSZ		; How many bytes.
	sta	REUTRANSLEN
	lda	#>BLKSZ
	sta	REUTRANSLEN+1
	lda	#<BLKSZ
	sta	REUTRANSLEN
	lda	#%10110000	; c64 → REU with immediate execution, autoload
	sta	REUCOMMAND
	lda	#<$d021
	sta	REUC64BASE
	lda	#>$d021
	sta	REUC64BASE+1
	lda	#%10000000
	sta	REUCONTROL
	@loop:
	lda	#%10110001	; c64 ← REU with immediate execution, autoload
	sta	REUCOMMAND
	jmp	@loop
