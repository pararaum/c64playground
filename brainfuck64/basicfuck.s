;;; Use the basic functions to create a brainfuck programm via the listing. Then use SAVE and LOAD normally.
	.include "LAMAlib-macros16.inc"
	.include "LAMAlib-structured.inc"
	.include "t7d/memoryfunctions.i"
	.include "t7d/basic.i"
	.include "t7d/kernal.i"
	.include "t7d/memoryconfig.i"
	.include "libbrainfuck/brainfuck.i"
	.include "colors.i"

	.import display_mode
	.import	__LOADADDR__
	.import	__BRAINFUCKCODE_START__
	.import install_irq
	.import _irq_catcher_init
	.import	__FONT__
	.import	__VICMEM_START__

	.exportzp ptr1,ptr2


	.zeropage
ptr1:	.res	2
ptr2:	.res	2

	.segment "LOADADDR"
	.word	__LOADADDR__

	.segment "INIT"
	.segment "EXEHDR"
	jmp	main
executor_entry:
	jmp	executor

	.segment "FONT"
	.incbin	"memdisplay_charset.bin"

	.code
;;; Copy a single basic line from ptr1 to ptr2, tokens are transformed back to the petscii characters.
;;; Input: ptr1,ptr2
;;; Output: ptr1,ptr2,Y=line length
;;; Modifies: A,X,Y,ptr1,ptr2
.proc	copy_line
	ldy	#0
l1:	lda	(ptr1),y
	beq	endl		; Zero means end.
	switch	A
	case	$b3		; <
	  lda	#'<'
	  bne	out
	case	$b1		; >
	  lda	#'>'
	  bne	out
	case	$aa		; +
	  lda	#'+'
	  bne	out
	case	$ab		; -
	  lda	#'-'
	  bne	out
	endswitch
out:	sta	(ptr2),y	; STA affects no flags!
	iny
	bne	l1
endl:	rts
.endproc

;;; Copy the bf code from the basic lines into the BRAINFUCKCODE area.
;;; Input: A/X=address of first line to be copied
;;; Output: A=0,Y=0
;;; Modifies: A,X,Y,ptr1,ptr2,...
.proc	copy_bf_to_code_area
	stax	ptr1		; Points to the first byte of the code
	poke	1,$34		;make all RAM visible
	ldax	#__BRAINFUCKCODE_START__
	stax	ptr2
cloop:	ldax	ptr1
	stax	tmpptr		; Store pointer to line
	clc
	adcax	#4		; Jump to the actual code
	stax	ptr1
	jsr	copy_line	; Now Y=length of line!
	tya			; Move lenght of line to A/X
	ldx	#0		; HI is zero, of course.
	clc
	adcax	ptr2		; Increment target pointer
	stax	ptr2		; And store.
	ldax	tmpptr		; Get original pointer to the line...
	stax	ptr1		; ...and restore the value.
	ldy	#1		; Load pointer to next line
	lda	(ptr1),y
	tax
	dey
	lda	(ptr1),y
	stax	ptr1		; And set pointer to the new line.
	lda	(ptr1),y	; Check LO of next pointer...
	iny
	ora	(ptr1),y	; ...check HI? Null Pointer?
	bne	cloop
	;; A=0 here.
	tay			; Y=0, see above
	sta	(ptr2),y	; Set end marker.

	lda	#$cf
	cmp	ptr2+1
	if	cs		;check if we still end before I/O area
	  poke	1,$36		;switch to memory configuration with BASIC ROM off but I/O and Kernal enabled
	endif
	rts
	.data
tmpptr:	.res	2
.endproc

	.code
;;; Copy the brainfuck code into $a000-$bfff.
;;; Input: A/X=address of first line to be copied
.proc	copy_n_run
	pha
	poke	shadow_d020, interpreter_running_color
	pla
	jsr	copy_bf_to_code_area
	;; Now RUN!
	;memoryconfig_kernal
	ldax	#__BRAINFUCKCODE_START__
	jsr	interpret_brainfuck
	poke	1,$37		; Restore memory configuration.
	poke	shadow_d020, editor_border
	bcc	finished
	lda	#<txt
	ldy	#>txt
	jsr	STROUT
finished:	rts
	.data
txt:	.byte	"Break",13,0
.endproc

	.code
;;; After tokenisation this routine is called via $308 in order to exectute the input buffer. We will use a poor man's interpreter and check only for the first token.
.proc	executor
	lda	$200		; Load first token from input buffer.
	switch	A
	case	$80		; END
	  jmp	64738
	case	$8a		; RUN
	  jsr	CHRGET		; Get next character.
	  jsr	CHRGET		; Get next character.
	  beq	nonc		; No next character.
	  jsr	$a96b		; Get decimal number into  into $14/$15
	  jsr	$a660		; CLR
	  jsr	$a8bc		; Search line from start memory pointer
	  ;; $7a contains now the address of the line
	  ldax	$7a		; Beginning of Basic code.
	  incax
	  jmp startrun
nonc:	  ldax	$2b		; Beginning of Basic code.
startrun:
	  jsr	copy_n_run
	  jmp	all_ok
	case	$a2		; NEW
	  ;lda	#$93
	  ;jsr	CHROUT		; clear screen
	  lda	#0		; Initialise Basic pointers.
	  jsr	$a642
	  jmp	all_ok
	case	$9b		; LIST
	  jmp	$a7e4		; Use the normal Executor.
	case	$93		; LOAD
	  jmp	$a7e4		; Use the normal Executor.
	case	$94		; SAVE
	  jmp	$a7e4		; Use the normal Executor.
	endswitch
	ldx	#$b		; SYNTAX Error
	jmp	($300)
all_ok:	ldx	#$FF		; No error.
	jmp	($300)
.endproc

	.code
.proc	warmstart
	jmp	$E38B
.endproc

;;; Setup the BF interpreter, used by both the BRK routine and main.
	.code
.proc	setup
	JSR	set_default_colors

	;; set BASIC end
	ldax	#__LOADADDR__
	stax	$37	;pointer to end of BASIC area
	;; Install NMI routine
	lda	#<nmi
	sta	$318
	lda	#>nmi
	sta	$319
	jsr	install_irq	; IRQ
	;; install IRQ catcher
	jsr	_irq_catcher_init
	ldax	#executor_entry	; IGONE routine, executes tokens.
	stax	$308
	ldax	#warmstart
	stax	$300
	rts
.endproc

	.code
;;; NMI routine, initialises screen, can be used to abort a running program.
.proc	nmi
	pha			; STore registers
	txa
	pha
	tya
	pha
	JSR	$F6BC		; Bump Clock
	JSR	$FFE1		; Test-Stop Vector
	beq	runstoprestore
	pla			; Retrieve registers
	tay
	pla
	tax
	pla
	rti
runstoprestore:
	JSR	$FDA3		; Initialise I/O
	JSR	$E518
	JSR	set_default_colors
	lda	#$0e		; lo/hi charset
	jsr	CHROUT
	jmp	($a002)		; Basic Warm Start
.endproc

	.code
.proc	main
	;; Clear the Basic area.
	lda	#0
	jsr	$a642		; Basic NEW; destroys the stack!
	pokew	$318,nmi	; Setup a new NMI routine.
	jsr	setup
	lda	#<bootmsg
	ldy	#>bootmsg
	jsr	STROUT
	ldx	#$80
	jmp	($a002)		; Warm start.
	.data

bootmsg:
	.byte	$e,$93
	.byte	"  **** BRAINFUCK 1.0 COMMODORE 64 ****",13,13
	.byte   " 2K cells reserved  28671 BF bytes free",13,13,13
	.byte   " Direct mode:load,save,list,run,new,end",13,13
	.byte   " Keys: F1/F3 toggle cell memory view",13
	.byte   "       F5 enable darkmode",13
	.byte   "       F7 change background color",13
	.byte   "       RUN/STOP stop program",13,0

.endproc

.proc set_default_colors
	lda	#editor_background
	sta	shadow_d021
	.if ::editor_border <> ::editor_background
	lda	#editor_border
	.endif
	sta	shadow_d020
	.if ::editor_border <> ::editor_cursorcolor
	lda	#editor_cursorcolor
	.endif
	sta	$0286
	poke	display_mode,0	;normal display mode
	rts
.endproc
