;;; -*- mode: asm -*-
;;; Memory configuration macros. [https://codebase64.pokefinder.org/doku.php?id=base:memory_management]

;☛	$01 value	$A000-BFFF	$D000-DFFF	$E000-FFFF	Notes
;☛	$30 +48 %000	RAM		RAM		RAM	
;☛	$31 +49 %001	RAM		CHARROM		RAM	
;☛	$32 +50 %010	RAM		CHARROM		KERNAL	
;☛	$33 +51 %011	BASIC		CHARROM		KERNAL	
;☛	$34 +52 %100	RAM		RAM		RAM	
;☛	$35 +53 %101	RAM		I/O		RAM	
;☛	$36 +54 %110	RAM		I/O		KERNAL	
;☛	$37 +55 %111	BASIC		I/O		KERNAL		Default

.macro	memoryconfig_ram
	lda	#$34
	sta	$1
.endmacro

.macro	memoryconfig_io
	lda	#$35
	sta	$1
.endmacro

.macro	memoryconfig_kernal
	lda	#$36
	sta	$1
.endmacro

.macro	memoryconfig_basic
	lda	#$37
	sta	$1
.endmacro

.macro	memoryconfig_charrom
	lda	#$31
	sta	$1
.endmacro
