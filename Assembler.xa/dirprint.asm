;;; Ripped from http://www.pagetable.com/?p=273
	* = $c000-2

	.word	$C000

	LDA #$01     ; filename length
	TAX
	LDY #$E8     ; there is a "$" at $E801 in ROM
	JSR $FFBD    ; set filename
	LDA #$60
	STA $B9      ; set secondary address
	JSR $F3D5    ; OPEN (IEC bus version)
	JSR $F219    ; set default input device
	LDY #$04     ; skip 4 bytes (load address and link pointer)
loop1	JSR $EE13    ; read byte
	DEY
	BNE loop1    ; loop
	LDA $90
	BNE chkeof    ; check end of file
	JSR $EE13    ; read byte (block count low)
	TAX
	JSR $EE13    ; read byte (block count high)
	JSR $BDCD    ; print 16 bit integer
loop2	JSR $EE13    ; read character
	JSR $FFD2    ; print character to stdout
	BNE loop2    ; loop until zero
	JSR $AAD7    ; print carriage return character
	LDY #$02
	BNE loop1    ; skip 2 bytes next time (link pointer)
chkeof	JSR $F642    ; CLOSE
	JMP $F6F3    ; reset default input device
