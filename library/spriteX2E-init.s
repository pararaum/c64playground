
	.include	"LAMAlib-macros16.inc"
	.export	sprX2E_init
	.globalzp SPRX2E_POINTER_CURRENT_WORKAREA

SPRX2E_POINTER_CURRENT_WORKAREA=sx2ep

	.zeropage
;;; SpriteX2Engine pointer. This points to the current engine work space.
sx2ep:	.res	2

	.code
	.proc	sprX2E_init
	stax	sx2ep		; Store pointer.
	tya			; Y=0: clear area.
	bne	noclear
	ldy	#8*4-1
l1:	sta	(sx2ep),y
	dey
	bpl	l1
noclear:
	rts
	.endproc
