;;; -*- mode: asm -*-
;;; Calculate the CRC-8. Remember that first make_crctable has to be called and then update_crc for each byte. The intermediate CRC is kept in the accumulator.

;;; Update the CRC value
;;; Input: A=value to update CRC with.
;;; Output: A=current CRC value.
;;; Modifies: A/X
	.import	update_crc

;;; This generates the CRC-Table to be used to calculate the CRC-8. This should be called first before calulating any CRC.
;;; Input: -
;;; Output: -
;;; Modifies: A/X/Y
	;; Required size is 256 Bytes.
	.import	make_crctable

;;; This constant contains the last calculated CRC-8.
	.importzp	CRC

;;; Clear the current CRC, this writes the current default CRCINITIAL into the CRC value.
;;; Input: -
;;; Output: -
;;; Modifies: -
	.import	clear_crc
