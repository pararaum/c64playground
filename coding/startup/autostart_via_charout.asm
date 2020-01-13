;;; xa -M -I ../include/ -l /dev/stdout autostart_via_charout.asm

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

main:	inc $d020
	clv
	dec $d020
	bvc main
	rts
