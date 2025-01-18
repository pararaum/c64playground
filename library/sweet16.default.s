	.importzp	SWEET16_REGSPACE

	.export	sweet16_default, sweet16_default_mainloop

R0L     =  SWEET16_REGSPACE + $0
R0H     =  SWEET16_REGSPACE + $1
R14H    =  SWEET16_REGSPACE + $1D
R15L    =  SWEET16_REGSPACE + $1E
R15H    =  SWEET16_REGSPACE + $1F

	.data
	;; Jumptables.
OPTBL:	.lobytes  SET          ;1X
BRTBL:	.lobytes  RTN          ;0
        .lobytes  LD           ;2X
        .lobytes  BR           ;1
        .lobytes  ST           ;3X
        .lobytes  BNC          ;2
        .lobytes  LDAT         ;4X
        .lobytes  BC           ;3
        .lobytes  STAT         ;5X
        .lobytes  BP           ;4
        .lobytes  LDDAT        ;6X
        .lobytes  BM           ;5
        .lobytes  STDAT        ;7X
        .lobytes  BZ           ;6
        .lobytes  POP          ;8X
        .lobytes  BNZ          ;7
        .lobytes  STPAT        ;9X
        .lobytes  BM1          ;8
        .lobytes  ADD          ;AX
        .lobytes  BNM1         ;9
        .lobytes  SUB          ;BX
        .lobytes  BK           ;A
        .lobytes  POPD         ;CX
        .lobytes  RS           ;B
        .lobytes  CPR          ;DX
        .lobytes  BS           ;C
        .lobytes  INR          ;EX
        .lobytes  NUL          ;D
        .lobytes  DCR          ;FX
        .lobytes  NUL          ;E
        .lobytes  NUL          ;UNUSED
        .lobytes  NUL          ;F

optbhi:	.hibytes  SET          ;1X
brtbhi:	.hibytes  RTN          ;0
        .hibytes  LD           ;2X
        .hibytes  BR           ;1
        .hibytes  ST           ;3X
        .hibytes  BNC          ;2
        .hibytes  LDAT         ;4X
        .hibytes  BC           ;3
        .hibytes  STAT         ;5X
        .hibytes  BP           ;4
        .hibytes  LDDAT        ;6X
        .hibytes  BM           ;5
        .hibytes  STDAT        ;7X
        .hibytes  BZ           ;6
        .hibytes  POP          ;8X
        .hibytes  BNZ          ;7
        .hibytes  STPAT        ;9X
        .hibytes  BM1          ;8
        .hibytes  ADD          ;AX
        .hibytes  BNM1         ;9
        .hibytes  SUB          ;BX
        .hibytes  BK           ;A
        .hibytes  POPD         ;CX
        .hibytes  RS           ;B
        .hibytes  CPR          ;DX
        .hibytes  BS           ;C
        .hibytes  INR          ;EX
        .hibytes  NUL          ;D
        .hibytes  DCR          ;FX
        .hibytes  NUL          ;E
        .hibytes  NUL          ;UNUSED
        .hibytes  NUL          ;F

	.code
sweet16_default:
        PLA
        STA  R15L           ;INIT SWEET16 PC
        PLA                 ;FROM RETURN
        STA  R15H           ;ADDRESS
sweet16_default_mainloop:
SW16B:	JSR  SW16C          ;INTERPRET AND EXECUTE
        JMP  SW16B          ;ONE SWEET16 INSTR.
SW16C:	INC  R15L
        BNE  SW16D          ;INCR SWEET16 PC FOR FETCH
        INC  R15H
SW16D:	LDY  #$0
        LDA  (R15L),Y       ;FETCH INSTR
        AND  #$F            ;MASK REG SPECIFICATION
        ASL                 ;DOUBLE FOR TWO BYTE REGISTERS
        TAX                 ;TO X REG FOR INDEXING
        LSR
        EOR  (R15L),Y       ;NOW HAVE OPCODE
        BEQ  TOBR           ;IF ZERO THEN NON-REG OP
        STX  R14H           ;INDICATE "PRIOR RESULT REG"
        LSR
        LSR                 ;OPCODE*2 TO LSB'S
        LSR
        TAY                 ;TO Y REG FOR INDEXING
	lda  optbhi-2,y	    ; high order adr bytes
	sta	jumpredirect+1
        LDA  OPTBL-2,Y      ;LOW ORDER ADR BYTE
        sta	jumpredirect
        jmp	$4c4c		; Placeholder!
	jumpredirect=*-2
TOBR:	INC  R15L
        BNE  TOBR2          ;INCR PC
        INC  R15H
TOBR2:	lda	brtbhi,x
	sta	jumpredirectbranch+1
	lda	BRTBL,X
        sta	jumpredirectbranch
        LDA  R14H           ;"PRIOR RESULT REG" INDEX
        LSR                 ;PREPARE CARRY FOR BC, BNC.
        jmp	$4c4c
	jumpredirectbranch=*-2
RTNZ:	PLA                 ;POP RETURN ADDRESS
        PLA
        JMP  (R15L)         ;RETURN TO 6502 CODE VIA PC
SETZ:	LDA  (R15L),Y       ;HIGH ORDER BYTE OF CONSTANT
        STA  R0H,X
        DEY
        LDA  (R15L),Y       ;LOW ORDER BYTE OF CONSTANT
        STA  R0L,X
        TYA                 ;Y REG CONTAINS 1
        SEC
        ADC  R15L           ;ADD 2 TO PC
        STA  R15L
        BCC  SET2
        INC  R15H
SET2:	RTS

SET:	jmp	SETZ           ;ALWAYS TAKEN
BK:	brk			; We need a BRK here as R0L may not be zero (Woz's version assumes this!).
	nop			; A NOP in case that the BRK routine did not fix the return address...
	;; Sweet16 branch-instructions already increment the 15th register, we need to turn this back!
	lda	R15L
	bne	nounderflow
	dec	R15H
nounderflow:
	dec	R15L
	rts
LD:	LDA  R0L,X
        STA  R0L
        LDA  R0H,X          ;MOVE RX TO R0
        STA  R0H
        RTS
ST:	LDA  R0L
        STA  R0L,X          ;MOVE R0 TO RX
        LDA  R0H
        STA  R0H,X
        RTS
STAT:	LDA  R0L
STAT2:	STA  (R0L,X)        ;STORE BYTE INDIRECT
        LDY  #$0
STAT3:	STY  R14H           ;INDICATE R0 IS RESULT NEG
INR:	INC  R0L,X
        BNE  INR2           ;INCR RX
        INC  R0H,X
INR2:	RTS
LDAT:	LDA  (R0L,X)        ;LOAD INDIRECT (RX)
        STA  R0L            ;TO R0
        LDY  #$0
        STY  R0H            ;ZERO HIGH ORDER R0 BYTE
        BEQ  STAT3          ;ALWAYS TAKEN
POP:	LDY  #$0             ;HIGH ORDER BYTE = 0
        BEQ  POP2           ;ALWAYS TAKEN
POPD:	JSR  DCR            ;DECR RX
        LDA  (R0L,X)        ;POP HIGH ORDER BYTE @RX
        TAY                 ;SAVE IN Y REG
POP2:	JSR  DCR            ;DECR RX
        LDA  (R0L,X)        ;LOW ORDER BYTE
        STA  R0L            ;TO R0
        STY  R0H
POP3:	LDY  #$0             ;INDICATE R0 AS LAST RESULT REG
        STY  R14H
        RTS
LDDAT:	JSR  LDAT           ;LOW ORDER BYTE TO R0, INCR RX
        LDA  (R0L,X)        ;HIGH ORDER BYTE TO R0
        STA  R0H
        JMP  INR            ;INCR RX
STDAT:	JSR  STAT           ;STORE INDIRECT LOW ORDER
        LDA  R0H            ;BYTE AND INCR RX. THEN
        STA  (R0L,X)        ;STORE HIGH ORDER BYTE.
        JMP  INR            ;INCR RX AND RETURN
STPAT:	JSR  DCR            ;DECR RX
        LDA  R0L
        STA  (R0L,X)        ;STORE R0 LOW BYTE @RX
        JMP  POP3           ;INDICATE R0 AS LAST RESULT REG
DCR:	LDA  R0L,X
        BNE  DCR2           ;DECR RX
        DEC  R0H,X
DCR2:	DEC  R0L,X
        RTS
SUB:	LDY  #$0             ;RESULT TO R0
CPR:	SEC            ;NOTE Y REG = 13*2 FOR CPR
        LDA  R0L
        SBC  R0L,X
        STA  R0L,Y          ;R0-RX TO RY
        LDA  R0H
        SBC  R0H,X
SUB2:	STA  R0H,Y
        TYA                 ;LAST RESULT REG*2
        ADC  #$0             ;CARRY TO LSB
        STA  R14H
        RTS
ADD:	LDA  R0L
        ADC  R0L,X
        STA  R0L            ;R0+RX TO R0
        LDA  R0H
        ADC  R0H,X
        LDY  #$0             ;R0 FOR RESULT
        BEQ  SUB2           ;FINISH ADD
BS:	LDA  R15L           ;NOTE X REG IS 12*2!
        JSR  STAT2          ;PUSH LOW PC BYTE VIA R12
        LDA  R15H
        JSR  STAT2          ;PUSH HIGH ORDER PC BYTE
BR:	CLC
BNC:	BCS  BNC2           ;NO CARRY TEST
BR1:	LDA  (R15L),Y       ;DISPLACEMENT BYTE
        BPL  BR2
        DEY
BR2:	ADC  R15L           ;ADD TO PC
        STA  R15L
        TYA
        ADC  R15H
        STA  R15H
BNC2:	RTS
BC:	BCS  BR
        RTS
BP:	ASL                 ;DOUBLE RESULT-REG INDEX
        TAX                 ;TO X REG FOR INDEXING
        LDA  R0H,X          ;TEST FOR PLUS
        BPL  BR1            ;BRANCH IF SO
        RTS
BM:	ASL                 ;DOUBLE RESULT-REG INDEX
        TAX
        LDA  R0H,X          ;TEST FOR MINUS
        BMI  BR1
        RTS
BZ:	ASL                 ;DOUBLE RESULT-REG INDEX
        TAX
        LDA  R0L,X          ;TEST FOR ZERO
        ORA  R0H,X          ;(BOTH BYTES)
        BEQ  BR1            ;BRANCH IF SO
        RTS
BNZ:	ASL                 ;DOUBLE RESULT-REG INDEX
        TAX
        LDA  R0L,X          ;TEST FOR NON-ZERO
        ORA  R0H,X          ;(BOTH BYTES)
        BNE  BR1            ;BRANCH IF SO
        RTS
BM1:	ASL                 ;DOUBLE RESULT-REG INDEX
        TAX
        LDA  R0L,X          ;CHECK BOTH BYTES
        AND  R0H,X          ;FOR $FF (MINUS 1)
        EOR  $FF
        BEQ  BR1            ;BRANCH IF SO
        RTS
BNM1:	ASL                 ;DOUBLE RESULT-REG INDEX
        TAX
        LDA  R0L,X
        AND  R0H,X          ;CHECK BOTH BYTES FOR NO $FF
        EOR  $FF
        BNE  BR1            ;BRANCH IF NOT MINUS 1
NUL:	RTS
RS:	LDX  #$18           ;12*2 FOR R12 AS STACK POINTER
        JSR  DCR            ;DECR STACK POINTER
        LDA  (R0L,X)        ;POP HIGH RETURN ADDRESS TO PC
        STA  R15H
        JSR  DCR            ;SAME FOR LOW ORDER BYTE
        LDA  (R0L,X)
        STA  R15L
        RTS
RTN:	JMP  RTNZ
	;; * End of single page code.
