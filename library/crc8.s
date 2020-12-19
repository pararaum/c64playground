;;; CRC-8 calculation, see http://www.6502.org/source/integers/crc.htm.
	CRCPOLYNOMIAL = $7
	CRCINITIAL = 0

	.import	CRCTABLE
	.export	update_crc
	.export	make_crctable
	.exportzp	CRC

	.zeropage
CRC:	.byte	CRCINITIAL

	.code
make_crctable:
	LDX	#0
@BYTELOOP:
	TXA		; A contains the CRC-8
	LDY	#8
@BITLOOP:
	ASL		 ; Shift CRC left
	BCC	@NOADD	 ; If no overflow, do nothing
	EOR	#CRCPOLYNOMIAL	; else add CRC-8 polynomial $07
@NOADD:	DEY
	BNE	@BITLOOP	; Do next bit
	STA	CRCTABLE,X	; All bits done, store in table
	INX
	BNE	@BYTELOOP	; Do next byte
	RTS

update_crc:
	EOR	CRC	; You really should inline this,
	TAX		; in which case you don't even need
	LDA	CRCTABLE,X	; the CRC location.
	sta	CRC
	RTS

clear_crc:
	pha
	lda	#CRCINITIAL
	sta	CRC
	pla
	rts
