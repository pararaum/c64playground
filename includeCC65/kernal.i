; -*- mode: asm -*-

;;; Commodore 64 standard KERNAL functions, taken from https://sta.c64.org/cbm64krnfunc.html.
;;; Also check: A.P. Stephenson, D.J. Stephenson, Advanced Machine Code Programming for the Commodore C64, Granada Publishing, 1984, Appendix B, page 222.
;;;                                         ⁂

; SCINIT. Initialize VIC; restore default input/output to keyboard/screen; clear screen; set PAL/NTSC switch and interrupt timer.
; Input: –
; Output: –
; Used registers: A, X, Y.
; Real address: $FF5B.
	SCINIT = $FF81


; IOINIT. Initialize CIA's, SID volume; setup memory configuration; set and start interrupt timer.
; Input: –
; Output: –
; Used registers: A, X.
; Real address: $FDA3.
	IOINIT = $FF84


; RAMTAS. Clear memory addresses $0002-$0101 and $0200-$03FF; run memory test and set start and end address of BASIC work area accordingly; set screen memory to $0400 and datasette buffer to $033C.
; Input: –
; Output: –
; Used registers: A, X, Y.
; Real address: $FD50.
	RAMTAS = $FF87


; RESTOR. Fill vector table at memory addresses $0314-$0333 with default values.
; Input: –
; Output: –
; Used registers: –
; Real address: $FD15.
	RESTOR = $FF8A


; VECTOR. Copy vector table at memory addresses $0314-$0333 from or into user table.
; Input: Carry: 0 = Copy user table into vector table, 1 = Copy vector table into user table; X/Y = Pointer to user table.
; Output: –
; Used registers: A, Y.
; Real address: $FD1A.
	VECTOR = $FF8D


; SETMSG. Set system error display switch at memory address $009D.
; Input: A = Switch value.
; Output: –
; Used registers: –
; Real address: $FE18.
	SETMSG = $FF90


; LSTNSA. Send LISTEN secondary address to serial bus. (Must call LISTEN beforehands.)
; Input: A = Secondary address.
; Output: –
; Used registers: A.
; Real address: $EDB9.
	LSTNSA = $FF93


; TALKSA. Send TALK secondary address to serial bus. (Must call TALK beforehands.)
; Input: A = Secondary address.
; Output: –
; Used registers: A.
; Real address: $EDC7.
	TALKSA = $FF96


; MEMBOT. Save or restore start address of BASIC work area.
; Input: Carry: 0 = Restore from input, 1 = Save to output; X/Y = Address (if Carry = 0).
; Output: X/Y = Address (if Carry = 1).
; Used registers: X, Y.
; Real address: $FE25.
	MEMBOT = $FF99


; MEMTOP. Save or restore end address of BASIC work area.
; Input: Carry: 0 = Restore from input, 1 = Save to output; X/Y = Address (if Carry = 0).
; Output: X/Y = Address (if Carry = 1).
; Used registers: X, Y.
; Real address: $FE34.
	MEMTOP = $FF9C


; SCNKEY. Query keyboard; put current matrix code into memory address $00CB, current status of shift keys into memory address $028D and PETSCII code into keyboard buffer.
; Input: –
; Output: –
; Used registers: A, X, Y.
; Real address: $EA87.
	SCNKEY = $FF9F


; SETTMO. Set serial bus timeout.
; Input: A = Timeout value <= 127, if bit 7 is set, disable timeout.
; Output: –
; Used registers: –
; Real address: $FE21.
	SETTMO = $FFA2


; IECIN. Read byte from serial bus. (Must call TALK and TALKSA beforehands.)
; Input: –
; Output: A = Byte read.
; Used registers: A.
; Real address: $EE13.
	IECIN = $FFA5


; IECOUT. Write byte to serial bus. (Must call LISTEN and LSTNSA beforehands.)
; Input: A = Byte to write.
; Output: –
; Used registers: –
; Real address: $EDDD.
	IECOUT = $FFA8


; UNTALK. Send UNTALK command to serial bus.
; Input: –
; Output: –
; Used registers: A.
; Real address: $EDEF.
	UNTALK = $FFAB
	

; UNLSTN. Send UNLISTEN command to serial bus.
; Input: –
; Output: –
; Used registers: A.
; Real address: $EDFE.
	UNLSTN = $FFAE


; LISTEN. Send LISTEN command to serial bus.
; Input: A = Device number.
; Output: –
; Used registers: A.
; Real address: $ED0C.
	LISTEN = $FFB1


; TALK. Send TALK command to serial bus.
; Input: A = Device number.
; Output: –
; Used registers: A.
; Real address: $ED09.
	TALK = $FFB4
	

; READST. Fetch status of current input/output device, value of ST variable. (For RS232, status is cleared.)
; Input: –
; Output: A = Device status.
; Used registers: A.
; Real address: $FE07.
	READST = $FFB7


; SETLFS. Set file parameters.
; Input: A = Logical number; X = Device number; Y = Secondary address.
; Output: –
; Used registers: –
; Real address: $FE00.
	SETLFS = $FFBA


; SETNAM. Set file name parameters.
; Input: A = File name length; X/Y = Pointer to file name.
; Output: –
; Used registers: –
; Real address: $FDF9.
	SETNAM = $FFBD


; OPEN. Open file. (Must call SETLFS and SETNAM beforehands.)
; Input: –
; Output: –
; Used registers: A, X, Y.
; Real address: ($031A), $F34A.
	OPEN = $FFC0


; CLOSE. Close file.
; Input: A = Logical number.
; Output: –
; Used registers: A, X, Y.
; Real address: ($031C), $F291.
	CLOSE = $FFC3


; CHKIN. Define file as default input. (Must call OPEN beforehands.)
; Input: X = Logical number.
; Output: –
; Used registers: A, X.
; Real address: ($031E), $F20E.
	CHKIN = $FFC6


; CHKOUT. Define file as default output. (Must call OPEN beforehands.)
; Input: X = Logical number.
; Output: –
; Used registers: A, X.
; Real address: ($0320), $F250.
	CHKOUT = $FFC9


; CLRCHN. Close default input/output files (for serial bus, send UNTALK and/or UNLISTEN); restore default input/output to keyboard/screen.
; Input: –
; Output: –
; Used registers: A, X.
; Real address: ($0322), $F333.
	CLRCHN = $FFCC


; CHRIN. Read byte from default input (for keyboard, read a line from the screen). (If not keyboard, must call OPEN and CHKIN beforehands.)
; Input: –
; Output: A = Byte read.
; Used registers: A, Y.
; Real address: ($0324), $F157.
	CHRIN = $FFCF


; CHROUT. Write byte to default output. (If not screen, must call OPEN and CHKOUT beforehands.)
; Input: A = Byte to write.
; Output: –
; Used registers: –
; Real address: ($0326), $F1CA.
	CHROUT = $FFD2


; LOAD. Load or verify file. (Must call SETLFS and SETNAM beforehands.)
; Input: A: 0 = Load, 1-255 = Verify; X/Y = Load address (if secondary address = 0).
; Output: Carry: 0 = No errors, 1 = Error; A = KERNAL error code (if Carry = 1); X/Y = Address of last byte loaded/verified (if Carry = 0).
; Used registers: A, X, Y.
; Real address: $F49E.
	LOAD = $FFD5
	

; SAVE. Save file. (Must call SETLFS and SETNAM beforehands.)
; Input: A = Address of zero page register holding start address of memory area to save; X/Y = End address of memory area plus 1.
; Output: Carry: 0 = No errors, 1 = Error; A = KERNAL error code (if Carry = 1).
; Used registers: A, X, Y.
; Real address: $F5DD.
	SAVE = $FFD8


; SETTIM. Set Time of Day, at memory address $00A0-$00A2.
; Input: A/X/Y = New TOD value.
; Output: –
; Used registers: –
; Real address: $F6E4.
	SETTIM = $FFDB

; RDTIM. read Time of Day, at memory address $00A0-$00A2.
; Input: –
; Output: A/X/Y = Current TOD value.
; Used registers: A, X, Y.
; Real address: $F6DD.
	RDTIM = $FFDE


; STOP. Query Stop key indicator, at memory address $0091; if pressed, call CLRCHN and clear keyboard buffer.
; Input: –
; Output: Zero: 0 = Not pressed, 1 = Pressed; Carry: 1 = Pressed.
; Used registers: A, X.
; Real address: ($0328), $F6ED.
	STOP = $FFE1


; GETIN. Read byte from default input. (If not keyboard, must call OPEN and CHKIN beforehands.)
; Input: –
; Output: A = Byte read.
; Used registers: A, X, Y.
; Real address: ($032A), $F13E.
	GETIN = $FFE4


; CLALL. Clear file table; call CLRCHN.
; Input: –
; Output: –
; Used registers: A, X.
; Real address: ($032C), $F32F.
	CLALL = $FFE7


; UDTIM. Update Time of Day, at memory address $00A0-$00A2, and Stop key indicator, at memory address $0091.
; Input: –
; Output: –
; Used registers: A, X.
; Real address: $F69B.
	UDTIM = $FFEA
	

; SCREEN. Fetch number of screen rows and columns.
; Input: –
; Output: X = Number of columns (40); Y = Number of rows (25).
; Used registers: X, Y.
; Real address: $E505.
	SCREEN  = $FFED


; PLOT. Save or restore cursor position.
; Input: Carry: 0 = Restore from input, 1 = Save to output; X = Cursor ROW (if Carry = 0); Y = Cursor COLUMN (if Carry = 0).
; Output: X = Cursor ROW (if Carry = 1); Y = Cursor COLUMN (if Carry = 1).
; Used registers: X, Y.
; Real address: $E50A.
	PLOT = $FFF0


; IOBASE. Fetch CIA #1 base address.
; Input: –
; Output: X/Y = CIA #1 base address ($DC00).
; Used registers: X, Y.
; Real address: $E500.
	IOBASE = $FFF3


;;; #############################################################################

;;; Other important kernal addresses.

;;; EXITIRQ. Restore registers and IRQ routine. Warning! This jump directly into the KERNAL.
;;; Input: -
;;; Output: -
;;; Never returns.
	EXITIRQ = $EA81

