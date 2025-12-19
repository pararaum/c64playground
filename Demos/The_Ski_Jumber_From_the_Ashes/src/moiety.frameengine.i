
	FRAMEQUEUES = 5

	.global	frameengine_init
	.global	frameengine_run
	.global	frameengine_frameno

	.struct	Framejob
	   frame	.word
	   queue	.byte
	   pointer	.word
	   accu		.byte
	.endstruct

;;; Add a job entry. The value of deltaframe is the number of frames to wait, so effects executed at the same time need a delay of zero. Use a deltaframe value of $FFFF to end the list in the initialisation phase.
;;; Input: AX=pointer to the list of effects
;;;	Y=0 convert deltas to absolute values, Y≠0 perform no conversion.
	.macro	JobEntry	deltaframe,queue,funcptr,accu
	.word	deltaframe
	.byte	queue
	.word	funcptr
	.byte	accu
	.endmacro
