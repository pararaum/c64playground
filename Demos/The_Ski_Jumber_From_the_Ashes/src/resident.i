; -*- mode: asm -*-

;;; ⚠ Warning! Resident area uses different zeropage addresses!

RESIDENTSTART=$800

;;; Setup the $FFFE/$FFFF vector for a simple routine that saves the registers, calls $1003 and returns.
	RESIDENT_set_default_irq = RESIDENTSTART

;;; Setup a call in the ISR to the address given in AX.
	RESIDENT_set_isr_call = RESIDENTSTART+3

;;; Decrunch XipZ qadz data.
	RESIDENT_decrunch_qadz = RESIDENTSTART+6

;;; Decrunch ZX02 data.
;;;
;;; Input: AX=start of compressed data, Stack=destination address (L/H)
	RESIDENT_decrunch_zx02 = RESIDENTSTART+9

;;; Load the next part (filename in AX), memory is set to RAM & I/O!
;;;
;;; Decompression is always done to address $2200 or in case of pucrunch into the load address specified in the pucrunched data.
;;;
;;; Input: AX=pointer to filename, Y=options (see below)
	RESIDENT_load_nextpart = RESIDENTSTART+12
	RESIDENT_loadE000 = 0 ; Load data into high area at $E000.
	RESIDENT_load8000 = 1 ; Load data into low area at $8000.
	RESIDENT_pucrunch = (1<<1)
	RESIDENT_zx02	  = (1<<2)
	RESIDENT_qadz	  = (1<<3)
	RESIDENT_keep_irq = (1<<4) ; Keep the current interrupt running, do not install the default interrupt.
	RESIDENT_load_RTS = %10000000 ; If set return via RTS and do *neither* decrunch *nor* jump to $2200

;;; Decrunch old loaded data with last options given to RESIDENT_load_nextpart, useful if RESIDENT_load_RTS was given.
	RESIDENT_decrunch_last_loaded = RESIDENTSTART+15

;;; Decrunch using pudecrunch data pointed to by AX.
	RESIDENT_pudecrunch = RESIDENTSTART+18

;;; Copy 1K (AX points to the data) to Datasette buffer and...
;;; Input:
;;;	AX=source pointer
;;;	Y=flags
	RESIDENT_copy_datasette = RESIDENTSTART+21
	.define	RESIDENT_datasette_jmp_33c       128    ; Jump to $33c after copying.
	.define RESIDENT_datasette_irq_33f       64     ; Set default resident interrupt handler and isr calls $33f.
	.define	RESIDENT_datasette_default_irq   32     ; Set default resident interrupt handler.
