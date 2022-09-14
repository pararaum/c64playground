;==========================================================
; Water Rising takeover effect include file
; Code by Wil 2022
; Version 0.1
; License: The Unlicense
;==========================================================

;;; -*-mode: asm -*-

;;; Pointer to the C64 screen which is to be dissolved.
;;; This is probably $0400 but may be changed if needed.
	.global	TAKEOVER_SCREENBASE

;;; Global variable that can be tested if the effect has finished. If
;;; the variable contains zero then the effect has finished.
	.global	takeover_water_rising

;;; Global variable that can be tested if the effect has finished. If
;;; the variable contains zero then the effect has finished.
	.global	takeover_water_level

;;; Init the takeover effect engine. *Must* be called first!
;;; Input: 
;;;	TAKEOVER_SCREENBASE: Address where the C64 screen is to be found.
;;; Modifies: A, X, Y
;;; Output: -
	.import	init_takeover_water_rising

;;; The update function must be called once per frame.
;;; Input: -
;;;	TAKEOVER_SCREENBASE_HIGH: Highbyte of aAddress of the text screen (usually 4, meaning screen is at $400)
;;; Modifies: A, X, Y
;;; Output: takeover_water_rising=0: effect finished
	.import	update_takeover_water_rising

