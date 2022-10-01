;;; -*- mode: asm -*- Brainfuck Interpreter
;;;
;;; Call the interpreter and interpret the Brainfuck code.


;;; Interpret code
;;; Input:
;;;	A/X=pointer to code
;;;	BRAINFUCKWORK=define this memory segment
;;; Output: C=1: aborted, C=0: finished normally
;;; Modifies: A, X, Y
;;;	BRAINFUCKWORK=memory segment can be changed by brainfuck code
	.import	interpret_brainfuck

