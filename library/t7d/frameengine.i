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
;;;
;;; Warning! If using the delta version of this function (with
;;; conversion) then remember that when things have to happen at the
;;; same time in different channels then only the first time a delay
;;; time must be given. The simultaneous actions in other channels
;;; need a delay of zero otherwise this is, of course, interpreted as
;;; another waiting time.
;;;
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
	.macro	FrameJob5CEntry	deltaframe,channel,funcptr,accu
	.word	deltaframe	; Delta or absulte frame number for next effect
	.byte	channel		; Which channel to use
	.word	funcptr		; Function pointer to be called
	.byte	accu		; Accumulator value for function call
	.endmacro

;;; -----------------------------------------------------------------------------
;;;
;;; The functions and macros below are for a different type of frame
;;; engine which has ability to set more register variables per
;;; channel.

	.global	frameengine_5c_allregs_init
	.global	frameengine_5c_allregs_run
	.global	frameengine_5c_allregs_frameno
	.struct	Framejob5Callregs
	   frame	.word
	   queue	.byte
	   pointer	.word
	   accu		.byte
	   xreg		.byte
	   yreg		.byte
	.endstruct

;;; Add a job entry. The value of deltaframe is the number of frames
;;; to wait, so effects executed at the same time need a delay of
;;; zero. Use a deltaframe value of $FFFF to end the list in the
;;; initialisation phase.
;;;
;;; If only "accu" is given then it is interpreted as a 16 bit value for A/X, Y is set to zero.
	.macro	FrameJob5CAllRegsEntry	deltaframe,channel,funcptr,accu,xreg,yreg
	.if .paramcount = 3	; Only function pointer
	.word	deltaframe	; Delta or absulte frame number for next effect
	.byte	channel		; Which channel to use
	.word	funcptr		; Function pointer to be called
	.byte	0		; A
	.byte	0		; X
	.byte	0		; Y
	.elseif .paramcount = 4
	;;  short form
	.word	deltaframe	; Delta or absulte frame number for next effect
	.byte	channel		; Which channel to use
	.word	funcptr		; Function pointer to be called
	.byte	<(accu)		; Accumulator value for function call
	.byte	>(accu)		; X register is set to HI of accu
	.byte	0		; Y register
	.elseif .paramcount = 5
	.word	deltaframe	; Delta or absulte frame number for next effect
	.byte	channel		; Which channel to use
	.word	funcptr		; Function pointer to be called
	.byte	<(accu)		; Accumulator value for function call
	.byte	>(accu)		; X register is set to HI of accu
	.byte	xreg		; Y register(!), as yreg is not given
	.else
	;; full form
	.word	deltaframe	; Delta or absulte frame number for next effect
	.byte	channel		; Which channel to use
	.word	funcptr		; Function pointer to be called
	.byte	accu		; Accumulator value for function call
	.byte	xreg		; X register
	.byte	yreg		; Y register
	.endif
	.endmacro

;;; Convenience function which can be used in the frameengine with all registers to poke something into memory.
;;; Input: AX=address to poke into, Y=value to poke into address AX
;;; Modifies: -
;;; Ouput: -
	.global	pokeAXcommaY
