; -*- mode: asm -*-

;;; The frameengine uses different channels, counts the number of
;;; frames and performs actions per channel on each invokation
;;; (aka. each frame). The first two channels are one-shot channels
;;; and after calling the corresponding function the function pointers
;;; are removed (HI only) so that next frame nothing is called. The
;;; next two channels are called and the function pointers are left
;;; unchanged so that they will be called in the next frame. The last
;;; channel is special as a CLI is executed so that other interrupts
;;; may occur while this long running job is executed. All other
;;; channels should finish as quickly as possible.
;;; 
;;; Frame#, channel, address
;;; Channels 0-1: one-shot (pointers deleted)
;;; Channels 2-3: called every frame
;;; Channel 4: continues running (but pointer deleted)


;;; Initialise the frame engine with 5 channels
;;; Input: AX=pointer to jobentry list, Y=0 convert deltas to absolute values, Y≠0 perform no conversion.
;;; Output: -
;;; Modifies: *
	.global	frameengine_5c_init

;;; Call each frame to execute the frame engine.
;;; Modifies: *
	.global	frameengine_5c_run

;;; Global variable (16bit) containing the frame number.
	.global	frameengine_5c_frameno

	.struct	Framejob5C
	   frame	.word
	   queue	.byte
	   pointer	.word
	   accu		.byte
	.endstruct

;;; Add a job entry. The value of deltaframe is the number of frames to wait, so effects executed at the same time need a delay of zero. Use a deltaframe value of $FFFF to end the list in the initialisation phase.
;;; Input: AX=pointer to the list of effects
;;;	Y=0 convert deltas to absolute values, Y≠0 perform no conversion.
	.macro	FrameJob5CEntry	deltaframe,channel,funcptr,accu
	.word	deltaframe	; Delta or absulte frame number for next effect
	.byte	channel		; Which channel to use
	.word	funcptr		; Function pointer to be called
	.byte	accu		; Accumulator value for function call
	.endmacro
