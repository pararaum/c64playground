	* = $033c
	
	LDA #$0E
	LDX #$08
	LDY #$00
	JSR $FFBA		;set up logical file
	LDA #$04
	LDX #<fname
	LDY #>fname
	JSR $FFBD		;set up filename
	JSR $FFC0		;open
	LDX #$0E
	JSR $FFC6		;CHKIN
	LDY #$00
	STY $0339		;$334-$33B empty
	STY $FA			;rs232 out buff
	STY $0338
	LDA $30
	STA $FE			;free
	CLC
	LDA $2F			;$2f/$30 begin basic vars
	ADC #$07
	BCC l36D
	INC $FE
l36D	STA $FD
	LDA #$00
	STA $FB
	LDA #$C0
	STA $FC
l377	LDY #$01
l379	JSR $FFCF		;CHRIN
	LDX $90			;STATUS
	BEQ l38F
	JSR $FFCC		;CLRCHN clear channel
	LDA #$0E
	JSR $FFC3		;CLOSE
	LDY $0338
	DEY
	STY $FE
	RTS
l38F	CPY #$06
	BCC l3C0
	CMP #$22
	BNE l3A1
	LDA $0339
	EOR #$01
	STA $0339
	BPL $03C0
l3A1	LDX $0339
	BNE l3C4
l3A6	CPY #$20
	BNE l3C0
	LDY #$00
	LDA $FA
	STA ($FD),Y
	STY $FA
	CLC
	LDA $FD
	ADC #$03
	BCC l3BB
	INC $FE
l3BB	STA $FD
	JMP l377
l3C0	INY
	JMP l379
l3C4	STY $033B
	LDY #$00
	STA ($FB),Y
	LDA $FA
	BNE l3DD
	LDY #$01
	LDA $FB
	STA ($FD),Y
	INY
	LDA $FC
	STA ($FD),Y
	INC $0338
l3DD	INC $FA
	LDY $033B
	INC $FB
	BNE l3E8
	INC $FC
l3E8	JMP l3A6
fname	.asc "$0:*"
