
	;;  Buffer at $500 seems to be available...
	.word	$500
	;;  Then size...
	.byte	program_end-main
	
main:
	rts
program_end:
