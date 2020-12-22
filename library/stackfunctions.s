
	.include	"stackmacros.i"
	.export	adjust_stackptr

.proc	adjust_stackptr
	sta	@tmp		; Store the delta for later use (self-modifying code).
	PullStoreStackptr
	lda	#0		; modified, see above
	@tmp = *-1
	AdjustStackptrA
	RetrievePushStackptr
	rts
.endproc
