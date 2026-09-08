	.include	"t7d/frameengine.i"


.proc	pokeAXcommaY
	sta	PTR
	stx	PTR+1
	sty	*
	PTR=*-2
	rts
.endproc
