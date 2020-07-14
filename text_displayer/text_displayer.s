;;; cl65 -m /dev/stderr  -C muzak.cfg text_displayer.s

	.segment "EXEHDR"

	.word	end_of_basic
	.word	7
	.byte	$9e,"(2064)"
end_of_basic:
	.byte	0,0,0
	nop
	jmp	main

	.data
	.byte "the 7th division"
	
	.code
main:
	rts
	
