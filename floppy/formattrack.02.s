;;; Format a track, the track number is used for writing the information onto the disk, a SEEK should be performed first? See R. Immers, G.G. Neufeld, "Inside Commodore DOS", Datamost, 1984, p. 103.
;;; Formatting code inspired by Gelfand, Felt, Strauch, Krsnik, "Das Anti-Crackerbuch", Data Becker, 1987, p. 248.
;;; 
;;; Run: x64 -drive8extend 2  -drivesound -drive8truedrive formattrack.01
	.include	"t7d/kernal.i"
	.include	"LAMAlib-macros16.inc"

current_track = $37		; Unused in 1541, "Die Floppy 1541", p.166.

	;; Job parameter. We use buffer 4 as formatting seems to kill buffer 5...
JOBCODE=1
JOBTRACK=$8
JOBSECTOR=$9
FLOPPYCODESTART=$400

	.zeropage
ptr1:	.res	2

	.data
mwcommand:
	.byte	"m-w"
mwcommand_addr:
	.word	FLOPPYCODESTART
mwcommand_len:
	.byte	0
mwcommand_buff:
	.res	32
mwcommand_end:

	.segment	"EXEHDR"
	.word	next
	.word	main
	.byte	$9e," ",$30+<((main/1000) .mod 10),$30+<((main/100) .mod 10),$30+<((main/10) .mod 10),$30+<(main .mod 10)
next:	.res	3
	.word	1

;;; Copy 32 Bytes to drive buffer at FLOPPYCODESTART
;;; Input: A/X=pointer to data
;;; Output: -
;;; Modifies: *

	.code
.proc	transferfloppy
	stax	ptr1
	sty	commlen
	lda	#8
	jsr	LISTEN		; Send listen to drive 8.
	lda	#$60+$f
	jsr	LSTNSA		; Listen secondary address, $60 is listen, $f is channel.
	ldy	#0
l2:	lda	(ptr1),y
	jsr	IECOUT
	iny
	cpy	#0
	commlen=*-1
	bne	l2
	jmp	UNLSTN		; Drive 8 can now stop listening.
.endproc

	.code
.proc copy32floppy
	ldy	#32
	sty	mwcommand_len
	dey
	stax	ptr1
l1:	lda	(ptr1),y
	sta	mwcommand_buff,y
	dey
	bpl	l1
	lda	#8
	jsr	LISTEN		; Send listen to drive 8.
	lda	#$60+$f
	jsr	LSTNSA		; Listen secondary address, $60 is listen, $f is channel.
	ldx	#0
l2:	lda	mwcommand,x
	jsr	IECOUT
	inx
	cpx	#mwcommand_end-mwcommand
	bne	l2
	jsr	UNLSTN		; Drive 8 can now stop listening.
	rts
.endproc

.proc	copy256floppy
	ldy	#8-1
	sty	datactr
	stax	dataptr
	lda	#0
	sta	mwcommand_addr
l1:	ldax	dataptr
	jsr	copy32floppy
	ldax	dataptr
	addax	#32
	stax	dataptr
	lda	mwcommand_addr
	clc
	adc	#$20
	sta	mwcommand_addr
	dec	datactr
	bpl	l1
	lda	#0
	sta	mwcommand_addr
	rts
	.pushseg
	.bss
dataptr:	.res	2
datactr:	.res	1
	.popseg
	rts
.endproc

	.rodata
drivecode:
	.org	FLOPPYCODESTART
	lda	#0
	STAGE=*-1
	asl
	asl
	sta	BRANCH
	inc	STAGE
	bne	*
	BRANCH=*-1
	nop
	jmp	floppymain
	nop
	jmp	format
	nop
	jmp	format
	nop
	jmp	format
	nop
	jmp	format
	nop
	jmp	format
	nop
	jmp	format
	nop
	jmp	format
	brk
floppymain:
	jsr	$d042		; Init Disk, read BAM. "Anatomy of the 1541", p. 154.
	lda	#36		; First track to be formatted.
	sta	current_track
@jobloop:
	sta	JOBTRACK	; Track for Job in buffer #2.
	lda	#$e0		; Execute Job...
	sta	JOBCODE		; ...in buffer ...
;	lda	#$b0		; SEEK, R. Immers et al, "Inside Commdore DOS", Datamost, 1984, p. 103.
;	sta	$3		; Job for Buffer #3.
;@job:	lda	$3
;	bmi	@job
@job:	lda	JOBCODE		; Job returned?
	bmi	@job
	inc	current_track	; Increment track number.
	lda	current_track
	cmp	#42
	bne	@jobloop
	lda	#1
	sta	JOBTRACK
	sta	JOBSECTOR
	lda	#$B0		; SEEK
	sta	JOBCODE
	rts
format:	lda	JOBTRACK	; Get track number.
	sta	$51		; Pass to format routine.
	lda	#$11		; Number of sectors.
	sta	$43		; Pass to format routine.
	lda	$1c00		; Get control port.
	and	#$9f
	ora	#0		; Set new speed.
	sta	$1c00
	jmp	$FAC7		; Call format routine.
	.reloc

floppyexecute:	.byte	"m-e",$0,>(FLOPPYCODESTART)
floppyexecute_end:

	.code
main:	nop
	ldax	#drivecode
	jsr	copy256floppy
	dec	$d020
	ldax	#floppyexecute
	ldy	#floppyexecute_end-floppyexecute
	jsr	transferfloppy
	rts
