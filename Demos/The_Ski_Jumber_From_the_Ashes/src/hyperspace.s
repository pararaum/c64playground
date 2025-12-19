.include        "t7d/memoryconfig.i"
.include        "t7d/memoryfunctions.i"
.include        "t7d/koala.i"
.include        "t7d/theatre/consts.i"
.include        "LAMAlib.inc"
.include        "resident.i"
.include "LAMAlib-sprites.inc"

        .rodata
nextpartname: .asciiz "crash.Z"

        .segment        "ONCE"
        .segment        "EXEHDR"
.export __EXEHDR__:absolute=1
        jsr     main
        jmp     nextpart
Start:

        .segment        "LOWCODE"
.proc nextpart
	lda	#$c8
	sta	$d016		; Restore to 40 columns.
        ldax    #nextpartname
        ldy     #RESIDENT_load8000|RESIDENT_pucrunch
        jmp     RESIDENT_load_nextpart
.endproc

.code
main:
        jsr     RESIDENT_set_default_irq
	memoryconfig_kernal

	.include "hyperspace/show.s"
