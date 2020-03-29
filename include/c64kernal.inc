; -*- mode: asm -*-
;;; See https://sta.c64.org/cbm64krnfunc.html
;;; See http://unusedino.de/ec64/technical/project64/mapping_c64.html
;✣  65409 ($FF81) CINT   (65371, $FF5B) initialize screen editor and video chip
;✣  65412 ($FF84) IOINIT (64931, $FDA3) initialize I/O devices
;✣  65415 ($FF87) RAMTAS (64848, $FD50) initialize RAM, tape buffer, screen
;✣  65418 ($FF8A) RESTOR (64789, $FD15) restore default I/O vectors
;✣  65421 ($FF8D) VECTOR (64794, $FD1A) read/set I/O vector table
;✣  65424 ($FF90) SETMSG (65048, $FE18) set Kernal message control flag
;✣  65427 ($FF93) SECOND (60857, $EDB9) send secondary address after LISTEN
;✣  65430 ($FF96) TKSA   (60871, $EDC7) send secondary address after TALK
;✣  65433 ($FF99) MEMTOP (65061, $FE25) read/set top of memory pointer
;✣  65436 ($FF9C) MEMBOT (65076, $FE34) read/set bottom of memory pointer
;✣  65439 ($FF9F) SCNKEY (60039, $EA87) scan the keyboard
;✣  65442 ($FFA2) SETTMO (65057, $FE21) set time-out flag for IEEE bus
;✣  65445 ($FFA5) ACPTR  (60947, $FE13) input byte from serial bus
;✣  65448 ($FFA8) CIOUT  (60893, $EDDD) output byte to serial bus
;✣  65451 ($FFAB) UNTLK  (60911, $EDEF) command serial bus device to UNTALK
;✣  65454 ($FFAE) UNLSN  (60926, $EDFE) command serial bus device to UNLISTEN
;✣  65457 ($FFB1) LISTEN (60684, $ED0C) command serial bus device to LISTEN
;✣  65460 ($FFB4) TALK   (60681, $ED09) command serial bus device to TALK
;✣  65463 ($FFB7) READST (65031, $FE07) read I/O status word
;✣  65466 ($FFBA) SETLFS (65024, $FE00) set logical file parameters
;✣  65469 ($FFBD) SETNAM (65017, $FDF9) set filename parameters
;✣  65472 ($FFC0) OPEN   (via 794 ($31A) to 62282, $F34A) open a logical file
;✣  65475 ($FFC3) CLOSE  (via 796 ($31C) to 62097, $F291) close a logical file
;✣  65478 ($FFC6) CHKIN  (via 798 ($31E) to 61966, $F20E) define an input channel
;✣  65481 ($FFC9) CHKOUT (via 800 ($320) to 62032, $F250) define an output channel
;✣  65484 ($FFCC) CLRCHN (via 802 ($322) to 62259, $F333) restore default devices
;✣  65487 ($FFCF) CHRIN  (via 804 ($324) to 61783, $F157) input a character
;✣  65490 ($FFD2) CHROUT (via 806 ($326) to 61898, $F1CA) output a character
;✣  65493 ($FFD5) LOAD   (via 816 ($330) to 62622, $F49E) load from device
;✣  65496 ($FFD8) SAVE   (via 818 ($332) to 62941, $F5DD) save to a device
;✣  65499 ($FFDB) SETTIM (63204, $F6E4) set the software clock
;✣  65502 ($FFDE) RDTIM  (63197, $F6DD) read the software clock
;✣  65505 ($FFE1) STOP   (via 808 ($328) to 63213, $F6ED) check the STOP key
;✣  65508 ($FFE4) GETIN  (via 810 ($32A) to 61758, $F13E) get a character
;✣  65511 ($FFE7) CLALL  (via 812 ($32C) to 62255, $F32F) close all files
;✣  65514 ($FFEA) UDTIM  (63131, $F69B) update the software clock
;✣  65517 ($FFED) SCREEN (58629, $E505) read number of screen rows and columns
;✣  65520 ($FFF0) PLOT   (58634, $E50A) read/set position of cursor on screen
;✣  65523 ($FFF3) IOBASE (58624, $E500) read base address of I/O devices


	;; Kernal vectors
	;; ═════════════


	;; Kernal routines
	;; ══════════════
	
	CHROUT = $FFD2
