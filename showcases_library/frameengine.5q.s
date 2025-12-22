
	.include	"t7d/frameengine.i"
	.include	"t7d/libt7d.i"
	.include	"t7d/vic/vicmacros.i"
	.include	"t7d/pseudo/loadstorereg.i"


	.data
framejobs:
	FrameJob5CEntry	0,2,incleft,0
	FrameJob5CEntry	0,3,incright,0
	FrameJob5CEntry	50,0,stabg,$c
	FrameJob5CEntry	50,0,stabg,$b
	FrameJob5CEntry	50,0,stabg,0
	;;
	FrameJob5CEntry	100,2,0,0
	FrameJob5CEntry	0,3,0,0
	FrameJob5CEntry	0,4,clear,221
	FrameJob5CEntry	100,4,clear,$20
	;;
	FrameJob5CEntry	25,2,incleft,0
	FrameJob5CEntry	25,3,incright,0
	FrameJob5CEntry	0,2,0,0
	FrameJob5CEntry	25,2,incleft,0
	FrameJob5CEntry	0,3,0,0
	FrameJob5CEntry	25,3,incright,0
	;;
	FrameJob5CEntry	10*50,0,64738,0
	FrameJob5CEntry	$FFFF,0,64738,0

	.code
.proc	stax10y10
	sta	$400+10*40+10
	rts
.endproc

.proc	stabg
	sta	$d021
	rts
.endproc

.proc	incleft
	inc	$400+40+1
	rts
.endproc

.proc	incright
	inc	$400+39+40-1
	rts
.endproc

.proc	clear
	ldx	#0
loop:
	.repeat	4,I
	 sta	$400+$100*I,x
	.endrepeat
	dex
	bne	loop
	rts
.endproc

	.code
irq:	asl	$d019
	jsr	frameengine_5c_run
	jmp	$ea31

	.segment	"EXEHDR"
	jsr	_disable_cia_irq
	sei
	SetIRQ314Pointer	irq
	EnableIRQatRasterline	251
	Load16	A,X,#framejobs
	ldy	#0
	jsr	frameengine_5c_init
	cli
mainloop:
	inc	$d020
	nop
	nop
	dec	$d020
	jmp	mainloop
