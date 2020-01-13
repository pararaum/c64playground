;;; xa -M -I ../include/ -l /dev/stdout parameter_after_call.asm

	COPYPTR = $FE

        .word $326
        * = $326        ;DO NOT CHANGE, else the autostart will fail
        .word boot      ;autostart from charout vector ($f1ca)
        .word $f6ed     ;$328 kernal stop routine Vector ($f6ed)
        .word $f13e     ;$32a kernal getin routine ($f13e)
        .word $f32f     ;$32c kernal clall routine vector ($f32f)
        .word $fe66     ;$32e user-defined vector ($fe66)
        .word $f4a5     ;$330 kernal load routine ($f4a5)
        .word $f5ed     ;$332 kernal save routine ($f5ed)

;* = $334 (cassette buffer)

boot    sei
        lda #$ca        ;repair charout vector ($f1ca)
        sta $326
        lda #$f1
        sta $327
	;; 
main:	lda #0
	jsr clear
	.word $0400+20		; Parameter one
	.word $0400+1000-20	; Parameter two
mloop:	inc $d020
	clv
	dec $d020
	bvc mloop
	rts

clear:	.(
	sta smod+1		; Store the clear value.
	tsx			; Get current stack pointer.
	lda $101,x		; Pointer, low.
	sta COPYPTR
	lda $102,x		; Pointer, high.
	sta COPYPTR+1
	ldy #1			; Remember JSR pushes address-1!
	;; Therefore we are here at address+2 (address-1 plus 1).
	lda (COPYPTR),y		; First parameter, low
	sta from+1
	iny
	lda (COPYPTR),y
	sta from+2		; First parameter, high
	iny
	lda (COPYPTR),y
	sta eptr1+1		; Second parameter, low.
	iny
	lda (COPYPTR),y		; Second parameter, high.
	sta eptr2+1
smod:	lda #$00		; Overwritten!
from:	sta $DEAD		; Overwritten with new address.
	inc from+1		; Increment pointer low.
	bne nopage
	inc from+2		; Next page.
nopage:	ldy from+1		; Low identical to stop value?
eptr1:	cpy #$ff
	bne from
	ldy from+2		; High identical to stop value
eptr2:	cpy #$ff
	bne from
	tsx			; Skip parameters on stack.
	lda #4			; Two words need to be skipped!
	clc
	adc $101,x
	sta $101,x
	lda #0
	adc $102,x
	sta $102,x
	rts
	.)
