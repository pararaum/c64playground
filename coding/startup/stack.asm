;;; xa -M -I ../include/ -l /dev/stdout stack.asm
;;; Stack is the area from $0100 to $01FF. Return address minus one are stored.

	.word	$0102		; Load at $0102.
	*=$0102
	sei			; Stop interrupts.
	inc $d020:dec $d020:jmp * ; Flicker and stop.
	.dsb $200-*,$01		; Fill stack up with $0101, so code at $0102 is started.
