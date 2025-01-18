; -*- mode: asm -*-

;;; This is the 32 byte register-space needed by the sweet16 engine. The space *must* be consecutive and in Zeropage.
	.globalzp	SWEET16_REGSPACE

;;; Call the SWEET16 engine. This engine does not save the registers!
;;; http://www.6502.org/source/interpreters/sweet16.htm
;;; https://en.wikipedia.org/wiki/SWEET16
;;; Input: sweet16 opcodes after the JSR
;;; Modifies: A,X,Y
;;; Output: -
	.global	sweet16_default
	.global	sweet16_default_mainloop
