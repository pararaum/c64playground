
	.import	_main
	.segment	"STARTUP"
	cld
	sei
	lda	#$36
	sta	1
	jsr	_main
	brk
