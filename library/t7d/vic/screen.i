
; -*- mode :asm -*-

;;; Switch the VIC screen off.
;;; Input: -
;;; Output: -
;;; Modifies: A
	.global	vic_screen_turn_off

;;; Switch the VIC screen on.
;;; Input: -
;;; Output: -
;;; Modifies: A
	.global	vic_screen_turn_on

;;; Switch the VIC screen off with mask ($d011 AND A).
;;; Input: A=mask
;;; Output: -
;;; Modifies: A
	.global	vic_screen_mask_turn_off

;;; Switch the VIC screen on with mask.
;;; Input: A=mask
;;; Output: -
;;; Modifies: A
	.global	vic_screen_mask_turn_on

;;; Switch the VIC screen off and keep MSB of raster off.
;;; Input: -
;;; Output: -
;;; Modifies: A
	.global	vic_screen_msb_turn_off

;;; Switch the VIC screen off and keep MSB of raster off.
;;; Input: -
;;; Output: -
;;; Modifies: A
	.global	vic_screen_msb_turn_on
