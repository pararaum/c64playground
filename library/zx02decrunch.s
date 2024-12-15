; De-compressor for ZX02 files
; ----------------------------
;
; Decompress ZX02 data (6502 optimized format), optimized for speed and size
;  138 bytes code, 58.0 cycles/byte in test file.
;
; Compress with:
;    zx02 input.bin output.zx0
;
; (c) 2022 DMSC
; Code under MIT license, see LICENSE file.

	.include	"t7d/pseudo/loadstorereg.i"
	.include	"t7d/stackmacros.i"

	.importzp	ptr1,ptr2,ptr3,ptr4,tmp1

ZX0_src	= ptr1
ZX0_dst = ptr2
offset	= ptr3
bitr	= tmp1
pntr	= ptr4

	.export	zx02decrunch_ip
	.export	zx02decrunch

	.code
.proc zx02decrunch_ip
	GetReturnAddrIntoPointer	ptr3
	ldy	#1
copy_parameter_loop:
	lda	(ptr3),y
	sta	ptr1-1,y
	iny
	cpy	#5
	bne	copy_parameter_loop
	PointerAdjustedToStack	ptr3, 4
;;; 	jmp	zx2decrunch
.endproc
;;; ⚠ Warning! Falls through to zx02decrunch. ⚠ 

;;; ⚠ See above for fall through code. ⚠
;--------------------------------------------------
; Decompress ZX0 data (6502 optimized format)
zx02decrunch:
	lda	#0
	sta	offset		; Clear the offset.
	sta	offset+1
	tay			; And clear Y!
	lda	#$80		; Reset bit counter(?).
	sta	bitr

; Decode literal: Ccopy next N bytes from compressed file
;    Elias(length)  byte[1]  byte[2]  ...  byte[N]
decode_literal:	
        jsr   get_elias

cop0:	lda   (ZX0_src), y
        inc   ZX0_src
        bne   @l50
        inc   ZX0_src+1
@l50:   sta   (ZX0_dst),y
        inc   ZX0_dst
        bne   @l54
        inc   ZX0_dst+1
@l54:	dex
        bne   cop0

        asl   bitr
        bcs   dzx0s_new_offset

; Copy from last offset (repeat N bytes from last offset)
;    Elias(length)
        jsr   get_elias
dzx0s_copy:
        lda   ZX0_dst
        sbc   offset  ; C=0 from get_elias
        sta   pntr
        lda   ZX0_dst+1
        sbc   offset+1
        sta   pntr+1

cop1:
        lda   (pntr), y
        inc   pntr
        bne   @l76
        inc   pntr+1
@l76:	sta   (ZX0_dst),y
        inc   ZX0_dst
        bne   @l80
        inc   ZX0_dst+1
@l80:	dex
        bne   cop1

        asl   bitr
        bcc   decode_literal

; Copy from new offset (repeat N bytes from new offset)
;    Elias(MSB(offset))  LSB(offset)  Elias(length-1)
dzx0s_new_offset:
				; Read elias code for high part of offset
        jsr   get_elias
        beq   exit  ; Read a 0, signals the end
				; Decrease and divide by 2
        dex
        txa
        lsr
        sta   offset+1

				; Get low part of offset, a literal 7 bits
        lda   (ZX0_src), y
        inc   ZX0_src
        bne   @l103
        inc   ZX0_src+1
@l103:
				; Divide by 2
        ror
        sta   offset

				; And get the copy length.
				; Start elias reading with the bit already in carry:
        ldx   #1
        jsr   elias_skip1

        inx
        bcc   dzx0s_copy

; Read an elias-gamma interlaced code.
; ------------------------------------
get_elias:
              ; Initialize return value to #1
        ldx   #1
        bne   elias_start

elias_get:	; Read next data bit to result
        asl   bitr
        rol
        tax

elias_start:	
              ; Get one bit
        asl   bitr
        bne   elias_skip1

              ; Read new bit from stream
        lda   (ZX0_src), y
        inc   ZX0_src
        bne   @l138
        inc   ZX0_src+1
@l138:             ;sec   ; not needed, C=1 guaranteed from last bit
        rol
        sta   bitr

elias_skip1:	
        txa
        bcs   elias_get
              ; Got ending bit, stop reading
exit:
        rts
