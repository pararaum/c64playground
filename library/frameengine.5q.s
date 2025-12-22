	.include	"t7d/frameengine.i"
	.include	"LAMAlib.inc"
	.include	"t7d/memoryfunctions.i"

FRAMEQUEUES=5

	.export	frameengine_5c_init
	.export	frameengine_5c_run
	.export	frameengine_5c_frameno

	.zeropage
jobqueueptr:	.res	2

	.bss
	;; Job Queues
queuedata_begin:
jq_funcptrLO:	.res	FRAMEQUEUES ; Pointer to the function to be called for this queue.
jq_funcptrHI:	.res	FRAMEQUEUES ; If this is zero then nothing is called!
jq_accus:	.res	FRAMEQUEUES ; Value loaded into A before calling the function.

frameengine_5c_frameno:	.res	2
queuedata_end:

current_queue:	.res	1	; Number of the queue currently handled.

	.code
.proc	frameengine_5c_init
	stax	jobqueueptr
	stax	jobqueueptr_keep ; Keep the jobqueue pointer save.
	cpy	#0		 ; Is Y equal to zero?
	bne	no_convert	 ; Nonzero? Then no conversion!
	lda	#0		 ; Set frame number to zero
	sta	frameengine_5c_frameno
	sta	frameengine_5c_frameno+1
convertloop:
	ldy	#0		; Get waiting time LO
	lda	(jobqueueptr),y
	pha			; Delta LO on stack, see below
	clc
	adc	frameengine_5c_frameno		; Add to frameengine_5c_frameno.
	sta	frameengine_5c_frameno
	sta	(jobqueueptr),y	; Now this is an absolute value.
	iny
	lda	(jobqueueptr),y
	pha			; Delta HI on stack, see below.
	adc	frameengine_5c_frameno+1
	sta	frameengine_5c_frameno+1
	sta	(jobqueueptr),y	; Now this is an absolute value.
	inc16	jobqueueptr,.sizeof(Framejob5C)
	pla			; Delta HI.
	tax
	pla			; Delta LO.
	cmp	#$FF
	bne	convertloop
	cpx	#$ff
	bne	convertloop
no_convert:
	;; Everything was converted, now the actual initialisation.
	SmallMemSet	0,queuedata_end-queuedata_begin,queuedata_begin
	ldax	jobqueueptr_keep
	stax	jobqueueptr
	lda	#0
	sta	frameengine_5c_frameno
	sta	frameengine_5c_frameno+1
	rts
	.pushseg
	.bss
jobqueueptr_keep:	.res	2
	.popseg
.endproc

.proc	check_if_frames_are_right
	ldy	#0		; Frame LO
	lda	(jobqueueptr),y
	cmp	frameengine_5c_frameno
	bne	out
	ldy	#1		; Frame HI
	lda	(jobqueueptr),y
	cmp	frameengine_5c_frameno+1
	bne	out
	ldy	#2		; Queue #
	lda	(jobqueueptr),y
	tax			; Queue # is now in X.
	ldy	#3		; Function pointer LO
	lda	(jobqueueptr),y
	sta	jq_funcptrLO,x
	ldy	#4		; Function pointer HI
	lda	(jobqueueptr),y
	sta	jq_funcptrHI,x
	ldy	#5		; Accu value
	lda	(jobqueueptr),y
	sta	jq_accus,x
	inc16	jobqueueptr,.sizeof(Framejob5C)
	jmp	check_if_frames_are_right ; Back to the beginning if multiple entries for the current frame.
out:	rts
.endproc

	.code
.proc	frameengine_5c_run
	lda	#0
	;; First two queues are one-shot.
	sta	jq_funcptrHI
	sta	jq_funcptrHI+1
	;; This is the long-time running queue.
	sta	jq_funcptrHI+4
	sta	current_queue
	jsr	check_if_frames_are_right
	inc16	frameengine_5c_frameno		; Increment frame number now, as long runners may interfere...
qloop:	ldx	current_queue
	cpx	#FRAMEQUEUES
	beq	out
	cpx	#FRAMEQUEUES-1	; Last queue is long running!
	bne	s1
	;; As we are last and we already incremented the frame number, we will finish the loop right now!
	cli
	lda	jq_funcptrHI,x
	beq	nojob_skip
	sta	JMPPTR+1
	lda	jq_funcptrLO,x
	sta	JMPPTR
	lda	jq_accus,x
	jmp	*
	JMPPTR=*-2		; We jump, no return here.
s1:
	lda	jq_funcptrHI,x
	beq	nojob_skip
	sta	JSRPTR+1
	lda	jq_funcptrLO,x
	sta	JSRPTR
	lda	jq_accus,x
	jsr	*
	JSRPTR=*-2
nojob_skip:
	inc	current_queue
	jmp	qloop
out:
	rts
.endproc
