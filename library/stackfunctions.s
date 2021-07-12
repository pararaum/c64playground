
	.include	"stackmacros.i"
	.export	adjust_stackptr

.proc	adjust_stackptr
	sta	@tmpdelta	; Store the delta for later use (self-modifying code).
	PullStoreStackptrLOCAL
	lda	#0		; modified, see above
	@tmpdelta = *-1
	AdjustStackptrA
	RetrievePushStackptrLOCAL
	rts
.endproc
