;;; xa -M -I ../include/ -l /dev/stdout 'sys_(2064).asm'

	.word	$0801
	*=$0801

	;; Just a simple BASIC header and 2064 is $0810.
	.word	end_of_basic	; 0801
	.word	main		; 0803
	.byte	$9e,$20,"(2064)" ; 0805
end_of_basic:
	.byte	0,0,0		; 080d

main:	inc $d020
	clv
	dec $d020
	bvc main
	rts
