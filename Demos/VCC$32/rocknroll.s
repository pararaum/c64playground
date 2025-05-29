	.include	"t7d/libt7d.i"
	.include	"t7d/vic/vicmacros.i"
	.include	"t7d/stackmacros.i"
	.include	"t7d/memoryconfig.i"
	.include	"LAMAlib.inc"
	.include	"moiety.rocknroll.i"

	.import	_frame0001, _frame0002, _frame0003, _frame0004

	.export	ROCKNROLL_TEXTSCREEN0 = $400
	.export	ROCKNROLL_TEXTSCREEN1 = $800
ROCKNROLL_SOURCESCREEN = _frame0002+2

	.export xpositions
	.rodata
xpositions:
	.repeat	32*8,I
	.word	I
	.endrepeat
	.word	$FFFF

	.data
framecounter:	.word	0

ypositions:
	;;  ','.join("%d"%(8*(6+6*math.sin(2*math.pi*p/100.0))) for p in range(100))
	;	.word	48,51,54,56,59,62,65,68,71,73,76,78,80,82,84,86,88,90,91,92,93,94,95,95,95,96,95,95,95,94,93,92,91,90,88,86,84,82,80,78,76,73,71,68,65,62,59,56,54,51,48,44,41,39,36,33,30,27,24,22,19,17,15,13,11,9,7,5,4,3,2,1,0,0,0,0,0,0,0,1,2,3,4,5,7,9,11,13,15,17,19,22,24,27,30,33,36,39,41,44
	;;  ','.join("%d"%(8*(6+5.9*math.sin(2*math.pi*p/100.0))-1) for p in range(100))
	.word   47,49,52,55,58,61,64,67,69,72,74,77,79,81,83,85,86,88,89,90,91,92,93,93,94,94,94,93,93,92,91,90,89,88,86,85,83,81,79,77,74,72,69,67,64,61,58,55,52,49,47,44,41,38,35,32,29,26,24,21,19,16,14,12,10,8,7,5,4,3,2,1,0,0,0,0,0,0,0,1,2,3,4,5,7,8,10,12,14,16,19,21,24,26,29,32,35,38,41,44
	.word	$FFFF
	
	.segment	"EXEHDR"
	sei
	memoryconfig_kernal
	PushWordJSR	#_main-1
	rts
	jmp	_main

	.code
.proc	setup_irq
	SetIRQ314Pointer	irq
	EnableIRQatRasterline	30
	rts
.endproc

irq:	asl	$d019
	jsr	$1003
	jsr	draw_rocknroll
	jsr	update_rocknroll
	inc16	framecounter
	jmp	$ea81

_main:	lda	#0
	sta	$d020
	sta	$d021
	jsr	$1000
	lda	#15
	jsr	_fill_colour_ram
	jsr	_disable_cia_irq
	lda	$d011
	and	#%11110111
	sta	$d011
	lda	$d016
	and	#%11110111
	sta	$d016
	jsr	setup_irq
	PushWordLH	#xpositions
	PushWordLH	#ypositions
	PushWordLH	#ROCKNROLL_SOURCESCREEN
	jsr	init_rocknroll
	cli
	ldy	#1
mainloop:
	lda	#2		; 5s * 2 = all 10s.
@wait:
	cmp	framecounter+1
	bne	@wait
	switch	Y
	case	1
	ldax	#_frame0001+2
	jsr	change_rocknroll
	ldy	#2
	break
	case	2
	ldax	#_frame0002+2
	jsr	change_rocknroll
	ldy	#3
	break
	case	3
	ldax	#_frame0003+2
	jsr	change_rocknroll
	ldy	#4
	break
	case	4
	ldax	#_frame0004+2
	jsr	change_rocknroll
	ldy	#1
	break
	endswitch
	lda	#0
	sta	framecounter+1

	lda framecolors-1,y
	for x,250,downto,1
	  .repeat 4,i
	    sta $d800+i*250-1,x
	  .endrep
	next	
	jmp	mainloop
framecolors:
	.byte 3,4,1,8 