;;; -*- mode: asm -*-

;;; Macros to load 16- and 24-bit values into multiple registers. The
;	registers may be chosen. This will make it easy to use foreign
;	code where 16-bit values are passed via A/Y, X/Y, etc.

	.IFNDEF	LOADSTOREREG_ALREADY_INCLUDED2024
	LOADSTOREREG_ALREADY_INCLUDED2024=1

;;; This macro stores 16-bit values at the given destination. reg1 and reg2 must be one of A, X, Y.
;;; Input:
;;;	- dest: destination address
;;;	- reg1: LO of value
;;;	- reg2: HI of value
	.macro	Store16	dest, reg1, reg2
	.if (.match(.left(1,{reg1}), a))
	sta	0+(dest)
	.elseif (.match(.left(1,{reg1}), x))
	stx	0+(dest)
	.elseif (.match(.left(1,{reg1}), y))
	sty	0+(dest)
	.else
	.error	"Register must be one of AXY!"
	.endif
	.if (.match(.left(1,{reg2}), a))
	sta	1+(dest)
	.elseif (.match(.left(1,{reg2}), x))
	stx	1+(dest)
	.elseif (.match(.left(1,{reg2}), y))
	sty	1+(dest)
	.else
	.error	"Register must be one of AXY!"
	.endif
	.endmacro

;;; This macro stores 24-bit values at the given destination. reg1 and reg2 must be one of A, X, Y.
;;; Input:
;;;	- dest: destination address
;;;	- reg1: LO of value
;;;	- reg2: HI of value
;;;	- reg3: Bank of value (bits 17 through 24)
	.macro	Store24	dest, reg1, reg2, reg3
	Store16	dest, reg1, reg2
	.if (.match(.left(1,{reg3}), a))
	sta	2+(dest)
	.elseif (.match(.left(1,{reg3}), x))
	stx	2+(dest)
	.elseif (.match(.left(1,{reg3}), y))
	sty	2+(dest)
	.else
	.error	"Register must be one of AXY!"
	.endif
	.endmacro

;;; This macro loads 16-bit values into the given registers. reg1 and reg2 must be one of A, X, Y.
;;; Input:
;;;	- src: source address or immediate value if it begins with '#'
;;;	- reg1: LO of value
;;;	- reg2: HI of value
	.macro	Load16	reg1, reg2, src
	.local srcHI, srcLO
	.if(.match(.left(1, {src}), #))
	.define srcLO #<(.right(.tcount({src})-1, {src}))
	.define srcHI #>(.right(.tcount({src})-1, {src}))
	.else
	.define srcLO 0+(src)
	.define srcHI 1+(src)
	.endif
	.if (.match(.left(1,{reg1}), a))
	lda	srcLO
	.elseif (.match(.left(1,{reg1}), x))
	ldx	srcLO
	.elseif (.match(.left(1,{reg1}), y))
	ldy	srcLO
	.else
	.error	"Register must be one of AXY!"
	.endif
	.if (.match(.left(1,{reg2}), a))
	lda	srcHI
	.elseif (.match(.left(1,{reg2}), x))
	ldx	srcHI
	.elseif (.match(.left(1,{reg2}), y))
	ldy	srcHI
	.else
	.error	"Register must be one of AXY!"
	.endif
	.endmacro

;;; This macro loads 24-bit values into the given registers. reg1 and reg2 must be one of A, X, Y.
;;; Input:
;;;	- src: source address or immediate value if it begins with '#'
;;;	- reg1: LO of value
;;;	- reg2: HI of value
;;;	- reg3: Bank of value (bits 17 through 24)
	.macro Load24 reg1, reg2, reg3, src
	.local	srcB
	.if(.match(.left(1, {src}), #))
	.define	srcB #((.right(.tcount({src})-1, {src}))>>16)
	Load16	reg1, reg2, #((.right(.tcount({src})-1, {src}))&$FFFF)
	.else
	.define	srcB 2+(src)
	Load16	reg1, reg2, (src&$FFFF)
	.endif
	.if (.match(.left(1,{reg3}), a))
	lda	srcB
	.elseif (.match(.left(1,{reg3}), x))
	ldx	srcB
	.elseif (.match(.left(1,{reg3}), y))
	ldy	srcB
	.else
	.error	"Register must be one of AXY!"
	.endif
	.endmacro
	
	.ENDIF
