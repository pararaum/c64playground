;;; -*- mode: asm -*-
;;; This is code/data which will be put into the LOWCODE segment. Warning! It also contains the copy routine and the LOWCODE will be garbled after initialisation. So beware!

	.include	"t7d/libt7d.i"
	.include	"t7d/memoryfunctions.i"

	.export	muzak_init, muzak_play, _copy_assets

muzak_init = MUZAK_START
muzak_play = muzak_init + 3

	.segment	"LOWCODE"
.proc _copy_assets
	nop
	CopyFileIntoMemoryDown	$400, TITLE_NAME, 2, 1000
	CopyFileIntoMemoryDown	$D800, TITLE_NAME, 1002, 1000
	nop
	.assert	* >= MUZAK_START, error
	CopyFileIntoMemoryDown	MUZAK_START, MUZAK_NAME, 2+$7c
	nop
	rts
.endproc
