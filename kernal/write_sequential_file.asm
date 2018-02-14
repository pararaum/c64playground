;;; Sample program to write a sequential (SEQ) file.
;;; See http://www.codebase64.org/doku.php?id=base:writing_a_file_byte-by-byte
	SETNAM = $FFBD
	SETLFS = $FFBA
	OPEN = $FFC0

	file_start = $0400
	file_end = $0800

	.WORD $0801
	* = $0801
	
basic:	.(
	.word end_of_basic
	.word main
	.byte $9e,$20,$c2
	.asc "(57)",$aa,"256",$ac,$c2,"(58)"
end_of_basic:
	.byte 0,0,0
	.)

main:	.(
	LDA #fname_end-fname
        LDX #<fname
        LDY #>fname
        JSR SETNAM

        LDA #$02      ; file number 2
        LDX $BA       ; last used device number
        BNE skip
        LDX #$08      ; default to device 8
skip:	LDY #$02      ; secondary address 2
        JSR $FFBA     ; call SETLFS

        JSR $FFC0     ; call OPEN
        BCS error    ; if carry set, the file could not be opened

        ; check drive error channel here to test for
        ; FILE EXISTS error etc.

        LDX #$02      ; filenumber 2
        JSR $FFC9     ; call CHKOUT (file 2 now used as output)

        LDA #<file_start
        STA $AE
        LDA #>file_start
        STA $AF

        LDY #$00
loop:	JSR $FFB7	; call READST (read status byte)
        BNE werror	; write error
        LDA ($AE),Y	; get byte from memory
        JSR $FFD2	; call CHROUT (write byte to file)
        INC $AE
        BNE skip2
        INC $AF
skip2
        LDA $AE
        CMP #<file_end
        LDA $AF
        SBC #>file_end
        BCC loop     ; next byte
close
        LDA #$02      ; filenumber 2
        JSR $FFC3     ; call CLOSE

        JSR $FFCC     ; call CLRCHN
        RTS
error:	pha
	lda #$02
	sta $d020
	pla
	sta $0400
	stx $0401
	sty $0402
        ; Accumulator contains BASIC error code

        ; most likely errors:
        ; A = $05 (DEVICE NOT PRESENT)

	;;         ... error handling for open errors ...
        JMP close    ; even if OPEN failed, the file has to be closed
werror:	pha
	lda #$00
	sta $d020
	pla
	sta $0400
	stx $0401
	sty $0402
	
        ; for further information, the drive error channel has to be read

	;;         ... error handling for write errors ...
        JMP close
	.)
fname:  .asc "OUTPUT,SEQ,W"
fname_end:
