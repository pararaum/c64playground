;;; xa -M -I ../include/ -l /dev/stdout 'sys_2078_the_7th_division.asm'

	.word	$0801
	*=$0801

	;; Just a simple BASIC header and 2064 is $0810.
	.word	end_of_basic	; 0801
	.word	$91d9		; 0803
	.byte	$9e,$20,"2080 n THE 7TH DIVISI",$91 ; DIVISION = DIVISI <token for ON>, yeah! n=╱ |=🮌 _=←
end_of_basic:
	.byte	0,0,0		; 080d

main:	inc $d020
	clv
	dec $d020
	bvc main
	rts
