	.import	_main
	.import	__BSS_LOAD__
	.import __BSS_SIZE__
	.macpack cbm

CLEAR_VALUE = 0

	.segment "LOADADDR"
	.export __LOADADDR__
__LOADADDR__:	.word $0801	; If this changes make sure to check the "crunch" and "run" section of the Makefile!

	.segment "STARTUP"
	.word	end_of_basic 	; Link to next line
	.word	2020		; Line number
	.byte	$9e,$20,$32,$30,$39,$36,$20,$3a,$20,$8f,$20,$54,$48,$45,$20,$37,$54,$48,$20,$44,$49,$56,$49,$53
	.byte	$49,$4f,$4e,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
end_of_basic:	.byte	$00,$00,$00
	cld
	jsr	clearbss
	jmp	_main

clearbss:
	lda	#<__BSS_LOAD__ ; Set pointer to the beginning of BSS
	sta	bssptr+1
	lda	#>__BSS_LOAD__
	sta	bssptr+2
	lda	#CLEAR_VALUE	; Clear value
	ldx	#0		; Clear X
	ldy	#>__BSS_SIZE__ ; Number of pages to clear
	beq	less_than_256
bssptr:	sta	$aaaa,x		; Clear a page
	dex
	bne	bssptr
	inc	bssptr+2	; Increment to next page.
	dey			; Decrement page counter
	bne	bssptr		; Loop
less_than_256:
	;;  See https://github.com/cc65/cc65/blob/master/libsrc/common/zerobss.s
	lda	bssptr+2	; Get high byte.
	sta	restptr+2	; Copy to the pointer to clear the rest.
	lda	#<__BSS_LOAD__ ; Pointer LO byte.
	sta	restptr+1
	lda	#CLEAR_VALUE	; Clear value
	ldx	#<__BSS_SIZE__ ; Remaining number of bytes.
	beq	out		  ; If zero then we are done.
restptr:	sta	$AAAA,x
	dex
	bne	restptr
out:	rts
