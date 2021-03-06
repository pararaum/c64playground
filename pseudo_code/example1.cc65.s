	.include	"pseudo16.inc"

	REGSRC	= $9b
	REGEND	= $a5
	REGDST	= $9e
	REGACC	= $a3

	.code
_main:	P_loadi	REGSRC,$a000
	P_loadi REGEND,$a000+1000
	P_loadi	REGDST,$0400
loop1:
	P_loadb REGSRC,REGACC
	P_storeb REGACC,REGDST
	P_inc	REGSRC
	P_inc	REGDST
	P_transfer	REGSRC,REGACC
	P_sub		REGEND,REGACC
	P_branchNZ	REGACC,loop1
	rts

	.segment "STARTUP"
	NOP
	jsr	_main
	NOP
	.ifdef __SIM6502__
	jmp	$FFF9
	.else
	rts
	.endif

	.segment "INIT"

	.segment "ONCE"

